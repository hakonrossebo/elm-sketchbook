module Sketches.Example4 exposing (Model, Msg, addComplex, init, iterateComplexMandelbrot, magnitudeComplex, subscriptions, update, view)

import Array exposing (Array)
import Browser.Dom as Dom
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onMouseMove, onResize)
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
        , error : Maybe String
        }



-- X resolution


xS =
    260



-- Y resolution


yS =
    200


xMin =
    -2.0


xMax =
    2.0


yMin =
    -2.0


yMax =
    2.0


maxMandelbrotIterations =
    50


zeroComplex =
    { r = 0, i = 0 }


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
            """
            }
    in
    ( { info = info
      , mousePosition = { x = 0, y = 0 }
      , pixels = Dict.empty
      , colorPercents = Dict.empty
      , sketchDrawingArea = Nothing
      , error = Nothing
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

        OnAnimationFrameDelta diff ->
            ( model, Cmd.none )

        OnMouseMove x y ->
            ( { model | mousePosition = { x = x, y = y } }, Cmd.none )

        OnSketchDrawingAreaFound element ->
            let
                pixels =
                    calculateMandelbrot (round (element.element.width / xS)) (round (element.element.height / yS))

                histogram =
                    pixels
                        |> Dict.toList
                        |> List.map (\( k, v ) -> v)
                        |> List.foldl createHistogram Dict.empty
                        |> Dict.toList

                histogramTotalIterations =
                    histogram
                        |> List.map (\( _, it ) -> it)
                        |> List.sum

                iterationPercents =
                    let
                        calcHistogramPercents : ( Int, Int ) -> ( Int, List ( Int, Float ) ) -> ( Int, List ( Int, Float ) )
                        calcHistogramPercents ( currentIterations, currentCount ) ( accCount, accList ) =
                            let
                                runningTotal =
                                    accCount + currentCount

                                percents =
                                    toFloat runningTotal
                                        / toFloat histogramTotalIterations

                                -- |> Debug.log "Percents:"
                                updatedList =
                                    ( currentIterations, percents ) :: accList
                            in
                            ( runningTotal, updatedList )
                    in
                    histogram
                        |> List.foldl calcHistogramPercents ( 0, [] )
                        |> (\( _, lst ) -> lst)
                        |> Dict.fromList
            in
            ( { model | sketchDrawingArea = Just element, pixels = pixels, colorPercents = iterationPercents }, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        OnWindowResize x y ->
            ( model, getSketchDrawingArea )

        OnKeyChange direction ->
            ( model, Cmd.none )


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
                        floor (element.element.width / xS)

                    windowHeight =
                        floor (element.element.height / yS)
                in
                [ viewMandelbrot windowWidth windowHeight model.pixels model.colorPercents ]

            Nothing ->
                [ text "Calculating Mandelbrot..."
                ]
        )


viewMandelbrot : Int -> Int -> Dict Point Int -> Dict Int Float -> Html Msg
viewMandelbrot xf yf pixels percents =
    List.range 0 yS
        |> List.map (viewMandelbrotRow pixels percents xf yf)
        |> div []


viewMandelbrotRow : Dict Point Int -> Dict Int Float -> Int -> Int -> Int -> Html Msg
viewMandelbrotRow pixels percents xf yf currentY =
    let
        yfString =
            String.fromInt yf ++ "px"
    in
    div
        [ style "height" yfString
        ]
        (List.range 0 xS
            |> List.map (viewPixel pixels percents xf yf currentY)
        )


linearInterpolation : Float -> Float -> Int -> Float
linearInterpolation color1 color2 t =
    color1 * (1 - toFloat t) + color2 * toFloat t


viewPixel : Dict Point Int -> Dict Int Float -> Int -> Int -> Int -> Int -> Html Msg
viewPixel pixels percents xf yf currentY currentX =
    let
        colorShade itr =
            -- 1 - toFloat itr / maxMandelbrotIterations
            1 - (1 - toFloat itr / 18)

        rgbColor =
            case Dict.get ( currentX, currentY ) pixels of
                Nothing ->
                    "black"

                Just iterations ->
                    rgb 0.0 0.0 (colorShade iterations)
                        |> toCssString

        -- rgbColorHist =
        --     case Dict.get ( currentX, currentY ) pixels of
        --         Nothing ->
        --             "black"
        --         Just iterations ->
        --             let
        --                 currentPercent =
        --                     case Dict.get iterations percents of
        --                         Nothing ->
        --                             1
        --                         Just p ->
        --                             p
        --                 rStart =
        --                     0.0
        --                 rChange =
        --                     0.1
        --                 gStart =
        --                     0.0
        --                 gChange =
        --                     0.1
        --                 bStart =
        --                     0.1
        --                 bChange =
        --                     0.9
        --                 r =
        --                     rStart + rChange * currentPercent
        --                 g =
        --                     gStart + gChange * currentPercent
        --                 b =
        --                     bStart + bChange * currentPercent
        --             in
        --             -- hsl h s l
        --             rgb r g b
        --                 |> toCssString
        -- hslColor =
        --     case Dict.get ( currentX, currentY ) pixels of
        --         Nothing ->
        --             hsl 0 0 0
        --                 |> toCssString
        --         Just iterations ->
        --             let
        --                 currentPercent =
        --                     case Dict.get iterations percents of
        --                         Nothing ->
        --                             0
        --                         Just p ->
        --                             -- Debug.log "P" p
        --                             p
        --                 h =
        --                     -- 0.5 - linearInterpolation currentPercent currentPercent (remainderBy (iterations + 1) 1)
        --                     0.7 - (0.7 - linearInterpolation currentPercent currentPercent (remainderBy (iterations + 1) 1))
        --                 -- 1 - currentPercent
        --                 -- 0.2
        --                 s =
        --                     0.6
        --                 l =
        --                     if iterations == 0 then
        --                         0
        --                     else
        --                         0.5
        --             in
        --             hsl h s l
        --                 |> toCssString
        xfString =
            String.fromInt xf ++ "px"

        yfString =
            String.fromInt yf ++ "px"
    in
    div
        [ style "width" xfString
        , style "height" yfString
        , style "background-color" rgbColor

        -- , style "background-color" hslColor
        -- , style "background-color" rgbColorHist
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


type alias ComplexNumber =
    { r : Float
    , i : Float
    }


addComplex : ComplexNumber -> ComplexNumber -> ComplexNumber
addComplex a b =
    { r = a.r + b.r
    , i = a.i + b.i
    }


magnitudeComplex : ComplexNumber -> Float
magnitudeComplex c =
    sqrt (c.r * c.r + c.i * c.i)


squareComplex : ComplexNumber -> ComplexNumber
squareComplex c =
    { r = c.r * c.r - c.i * c.i
    , i = 2.0 * c.r * c.i
    }


iterateComplexMandelbrot : ComplexNumber -> ComplexNumber -> Int -> Int -> Int
iterateComplexMandelbrot prevComplex currentComplex maxIterations currentIteration =
    if currentIteration > maxIterations then
        0

    else if magnitudeComplex currentComplex > 2 then
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


calculateMandelbrot : Int -> Int -> Dict Point Int
calculateMandelbrot width height =
    let
        xFactor =
            (xMax - xMin) / toFloat xS

        yFactor =
            (yMax - yMin) / toFloat yS

        calculateRow row mdict =
            List.range 0 xS
                |> List.foldl (calculateMandelbrotPixel xFactor yFactor row) mdict

        rows =
            List.range 0 yS
                |> List.foldl calculateRow Dict.empty
    in
    rows


calculateMandelbrotPixel : Float -> Float -> Int -> Int -> Dict Point Int -> Dict Point Int
calculateMandelbrotPixel xFactor yFactor y x pixels =
    let
        iterations =
            iterateComplexMandelbrot (toComplex xFactor yFactor y x) (toComplex xFactor yFactor y x) maxMandelbrotIterations 1

        newDict =
            if iterations == 0 then
                pixels

            else
                Dict.insert ( x, y ) iterations pixels
    in
    newDict


toComplex : Float -> Float -> Int -> Int -> ComplexNumber
toComplex xFactor yFactor y x =
    let
        r =
            toFloat x * xFactor + xMin

        i =
            toFloat y * yFactor + yMin
    in
    { r = r, i = i }
