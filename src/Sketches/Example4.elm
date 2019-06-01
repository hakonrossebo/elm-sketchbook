module Sketches.Example4 exposing (Model, Msg, addComplex, init, iterateComplexMandelbrot, subscriptions, update, view)

import Array exposing (Array)
import Browser.Dom as Dom
import Browser.Events exposing (onAnimationFrameDelta, onClick, onKeyDown, onMouseMove, onResize)
import Color exposing (..)
import Dict exposing (Dict)
import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class, id, style)
import Json.Decode as Decode
import Shared exposing (..)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, viewBox, width)
import Task


type alias Model =
    SharedModel
        { mousePosition : Position
        , sketchDrawingArea : Maybe Dom.Element
        , pixels : Dict Point Int
        , colorPercents : Dict Int Float
        , mandelbrotParameters : MandelbrotParameters
        , error : Maybe String
        }


type alias MandelbrotParameters =
    { xResolution : Int
    , yResolution : Int
    , reMin : Float
    , reMax : Float
    , imMin : Float
    , imMax : Float
    , maxMandelbrotIterations : Int
    }


zeroComplex =
    { re = 0, im = 0 }


type alias ComplexNumber =
    { re : Float
    , im : Float
    }


type alias Position =
    { x : Float
    , y : Float
    }


type alias Point =
    ( Int, Int )


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


type Msg
    = NoOp
    | OnMouseClick
    | OnAnimationFrameDelta Float
    | OnMouseMove Float Float
    | OnSketchDrawingAreaFound Dom.Element
    | OnWindowResize Int Int
    | OnError String
    | OnKeyChange Direction


init : ( Model, Cmd Msg )
init =
    let
        info =
            { title = "Mandelbrot"
            , markdown = """
Mandelbrot example. To avoid too many pixels, a fixed resolution is used.

You can click in the image to zoom the fractal.
            """
            }

        mandelbrotParameters =
            { xResolution = 260
            , yResolution = 200
            , reMin = -2.0
            , reMax = 2.0
            , imMin = -2.0
            , imMax = 2.0
            , maxMandelbrotIterations = 150
            }
    in
    ( { info = info
      , mousePosition = { x = 0, y = 0 }
      , pixels = Dict.empty
      , colorPercents = Dict.empty
      , sketchDrawingArea = Nothing
      , error = Nothing
      , mandelbrotParameters = mandelbrotParameters
      }
    , getSketchDrawingArea
    )


getSketchDrawingArea : Cmd Msg
getSketchDrawingArea =
    let
        processElement e =
            case e of
                Ok result ->
                    OnSketchDrawingAreaFound result

                Err error ->
                    case error of
                        Dom.NotFound errorInfo ->
                            OnError errorInfo
    in
    Dom.getElement "sketch-drawing-area"
        |> Task.attempt processElement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnMouseClick ->
            let
                mouseComplexPos =
                    case model.sketchDrawingArea of
                        Just element ->
                            mouseposToComplex model.mousePosition element model.mandelbrotParameters

                        Nothing ->
                            zeroComplex

                newZoomedParameters =
                    applyZoom model.mandelbrotParameters mouseComplexPos 1.4

                pixels =
                    calculateMandelbrot model.mandelbrotParameters
            in
            ( { model | mandelbrotParameters = newZoomedParameters, pixels = pixels }, Cmd.none )

        OnAnimationFrameDelta diff ->
            ( model, Cmd.none )

        OnMouseMove x y ->
            ( { model | mousePosition = { x = x, y = y } }, Cmd.none )

        OnSketchDrawingAreaFound element ->
            let
                pixels =
                    calculateMandelbrot model.mandelbrotParameters
            in
            ( { model | sketchDrawingArea = Just element, pixels = pixels }, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        OnWindowResize x y ->
            ( model, getSketchDrawingArea )

        OnKeyChange direction ->
            ( model, Cmd.none )


applyZoom : MandelbrotParameters -> ComplexNumber -> Float -> MandelbrotParameters
applyZoom parameters pos zoomFactor =
    let
        i =
            1.0 / zoomFactor

        reMin =
            interpolateM pos.re parameters.reMin i

        reMax =
            interpolateM pos.re parameters.reMax i

        imMin =
            interpolateM pos.im parameters.imMin i

        imMax =
            interpolateM pos.im parameters.imMax i
    in
    { parameters | reMin = reMin, reMax = reMax, imMin = imMin, imMax = imMax }


interpolateM : Float -> Float -> Float -> Float
interpolateM start end i =
    start + ((end - start) * i)


mouseposToComplex : Position -> Dom.Element -> MandelbrotParameters -> ComplexNumber
mouseposToComplex mousePosition element mandelbrotParameters =
    let
        xWindowPos =
            mousePosition.x - element.element.x

        yWindowPos =
            mousePosition.y - element.element.y

        reWidth =
            element.element.width / (mandelbrotParameters.reMax - mandelbrotParameters.reMin)

        imHeight =
            element.element.height / (mandelbrotParameters.imMax - mandelbrotParameters.imMin)

        re =
            xWindowPos / reWidth + mandelbrotParameters.reMin

        im =
            yWindowPos / imHeight + mandelbrotParameters.imMin
    in
    { re = re, im = im }


createHistogram : Int -> Dict Int Int -> Dict Int Int
createHistogram iterations histogram =
    case Dict.get iterations histogram of
        Nothing ->
            Dict.insert iterations 1 histogram

        Just currentHistogramIterations ->
            Dict.insert iterations (currentHistogramIterations + 1) histogram


keyDecoder : Decode.Decoder Direction
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> Direction
toDirection string =
    case string of
        "ArrowLeft" ->
            Left

        "ArrowRight" ->
            Right

        "ArrowUp" ->
            Up

        "ArrowDown" ->
            Down

        _ ->
            Other


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        offsetXDecoder : Decode.Decoder Float
        offsetXDecoder =
            Decode.field "clientX" Decode.float

        offsetYDecoder : Decode.Decoder Float
        offsetYDecoder =
            Decode.field "clientY" Decode.float
    in
    Sub.batch
        [ onMouseMove (Decode.map2 OnMouseMove offsetXDecoder offsetYDecoder)
        , onClick (Decode.succeed OnMouseClick)
        , onResize OnWindowResize
        , onKeyDown (Decode.map OnKeyChange keyDecoder)
        ]


view : Model -> Html Msg
view model =
    case model.error of
        Nothing ->
            div [ class "sketch-default-container" ]
                [ div [ class "sketch-default-top-item" ] [ text "Mandelbrot fractal example" ]
                , viewSketchDrawingContent model
                , viewMousePositionInformation model
                ]

        Just error ->
            div [ class "sketch-default-container" ]
                [ viewError error
                ]


viewError : String -> Html Msg
viewError error =
    div []
        [ h1 [] [ text error ]
        ]


viewSketchDrawingContent : Model -> Html Msg
viewSketchDrawingContent model =
    div [ class "sketch-default-main-item", id "sketch-drawing-area" ]
        (case model.sketchDrawingArea of
            Just element ->
                let
                    windowWidth =
                        floor (element.element.width / toFloat model.mandelbrotParameters.xResolution)

                    windowHeight =
                        floor (element.element.height / toFloat model.mandelbrotParameters.yResolution)
                in
                [ viewMandelbrot model.mandelbrotParameters windowWidth windowHeight model.pixels model.colorPercents ]

            Nothing ->
                [ text "Calculating Mandelbrot..."
                ]
        )


viewMandelbrot : MandelbrotParameters -> Int -> Int -> Dict Point Int -> Dict Int Float -> Html Msg
viewMandelbrot parameters xPixelSize yPixelSize pixels percents =
    List.range 0 parameters.yResolution
        |> List.map (viewMandelbrotRow parameters pixels percents xPixelSize yPixelSize)
        |> div []


viewMandelbrotRow : MandelbrotParameters -> Dict Point Int -> Dict Int Float -> Int -> Int -> Int -> Html Msg
viewMandelbrotRow parameters pixels percents xPixelSize yPixelSize currentY =
    let
        yPixelSizeString =
            String.fromInt yPixelSize ++ "px"
    in
    div
        [ style "height" yPixelSizeString
        ]
        (List.range 0 parameters.xResolution
            |> List.map (viewPixel parameters pixels percents xPixelSize yPixelSize currentY)
        )


linearInterpolation : Float -> Float -> Int -> Float
linearInterpolation color1 color2 t =
    color1 * (1 - toFloat t) + color2 * toFloat t


viewPixel : MandelbrotParameters -> Dict Point Int -> Dict Int Float -> Int -> Int -> Int -> Int -> Html Msg
viewPixel parameters pixels percents xPixelSize yPixelSize currentY currentX =
    let
        colorShade itr =
            1 - (1 - toFloat itr / 22)

        rgbColor =
            case Dict.get ( currentX, currentY ) pixels of
                Nothing ->
                    "black"

                Just iterations ->
                    rgb 0.0 0.0 (colorShade iterations)
                        |> toCssString

        xPixelSizeString =
            String.fromInt xPixelSize ++ "px"

        yPixelSizeString =
            String.fromInt yPixelSize ++ "px"
    in
    div
        [ style "width" xPixelSizeString
        , style "height" yPixelSizeString
        , style "background-color" rgbColor
        , style "display" "inline-block"
        ]
        []


viewMousePositionInformation : Model -> Html Msg
viewMousePositionInformation model =
    case model.sketchDrawingArea of
        Just element ->
            let
                mouseX =
                    model.mousePosition.x
                        |> round
                        |> String.fromInt
                        |> (\n -> "Mouse X: " ++ n)

                mouseY =
                    model.mousePosition.y
                        |> round
                        |> String.fromInt
                        |> (\n -> "Mouse Y: " ++ n)

                windowWidth =
                    element.element.width
                        |> round
                        |> String.fromInt
                        |> (\n -> "Window width: " ++ n)

                windowHeight =
                    element.element.height
                        |> round
                        |> String.fromInt
                        |> (\n -> "Window height: " ++ n)
            in
            div [ class "sketch-default-footer-item" ]
                [ span [] [ text mouseX ]
                , span [] [ text mouseY ]
                , span [] [ text windowWidth ]
                , span [] [ text windowHeight ]
                ]

        Nothing ->
            div [ class "sketch-default-footer-item" ]
                [ h1 [] [ text "Sketch drawing area not ready" ]
                ]



-- Mandelbrot calculations


addComplex : ComplexNumber -> ComplexNumber -> ComplexNumber
addComplex a b =
    { re = a.re + b.re
    , im = a.im + b.im
    }


magnitudeComplexNoSqrt : ComplexNumber -> Float
magnitudeComplexNoSqrt c =
    c.re * c.re + c.im * c.im


squareComplex : ComplexNumber -> ComplexNumber
squareComplex c =
    { re = c.re * c.re - c.im * c.im
    , im = 2.0 * c.re * c.im
    }


iterateComplexMandelbrot : ComplexNumber -> ComplexNumber -> Int -> Int -> Int
iterateComplexMandelbrot prevComplex currentComplex maxIterations currentIteration =
    if currentIteration > maxIterations then
        0

    else if magnitudeComplexNoSqrt currentComplex > 4 then
        currentIteration

    else
        let
            newComplex =
                currentComplex
                    |> squareComplex
                    |> addComplex prevComplex

            nextIteration =
                currentIteration + 1
        in
        iterateComplexMandelbrot prevComplex newComplex maxIterations nextIteration


calculateMandelbrot : MandelbrotParameters -> Dict Point Int
calculateMandelbrot parameters =
    let
        xFactor =
            (parameters.reMax - parameters.reMin) / toFloat parameters.xResolution

        yFactor =
            (parameters.imMax - parameters.imMin) / toFloat parameters.yResolution

        calculateRow row mdict =
            List.range 0 parameters.xResolution
                |> List.foldl (calculateMandelbrotPixel parameters xFactor yFactor row) mdict

        rows =
            List.range 0 parameters.yResolution
                |> List.foldl calculateRow Dict.empty
    in
    rows


calculateMandelbrotPixel : MandelbrotParameters -> Float -> Float -> Int -> Int -> Dict Point Int -> Dict Point Int
calculateMandelbrotPixel parameters xFactor yFactor y x pixels =
    let
        iterations =
            iterateComplexMandelbrot (toComplex parameters xFactor yFactor y x) (toComplex parameters xFactor yFactor y x) parameters.maxMandelbrotIterations 1

        newDict =
            if iterations == 0 then
                pixels

            else
                Dict.insert ( x, y ) iterations pixels
    in
    newDict


toComplex : MandelbrotParameters -> Float -> Float -> Int -> Int -> ComplexNumber
toComplex parameters xFactor yFactor y x =
    let
        re =
            toFloat x * xFactor + parameters.reMin

        im =
            toFloat y * yFactor + parameters.imMin
    in
    { re = re, im = im }
