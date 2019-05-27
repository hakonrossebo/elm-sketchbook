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
        , error : Maybe String
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
            { title = "Template 1"
            , markdown = """
Template with mouse, keyboard, window and animationframe messages. Uses the default sketch layout with a drawing area.
            """
            }
    in
    ( { info = info
      , mousePosition = { x = 0, y = 0 }
      , pixels = Dict.empty
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
                    calculateMandelbrot (round element.element.width) (round element.element.height)

                -- |> Debug.log "Mandelbrot"
            in
            ( { model | sketchDrawingArea = Just element, pixels = pixels }, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        OnWindowResize x y ->
            ( model, getSketchDrawingArea )

        OnKeyChange direction ->
            ( model, Cmd.none )


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
        [ onAnimationFrameDelta OnAnimationFrameDelta
        , onMouseMove (Decode.map2 OnMouseMove offsetXDecoder offsetYDecoder)
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
                        element.element.width
                            |> round

                    windowHeight =
                        element.element.height
                            |> round
                in
                [ viewMandelbrot windowWidth windowHeight model.pixels ]

            Nothing ->
                [ text "Drawing area not ready"
                ]
        )


viewMandelbrot : Int -> Int -> Dict Point Int -> Html Msg
viewMandelbrot width height pixels =
    List.range 0 height
        |> List.map (viewMandelbrotRow pixels width)
        |> div []


viewMandelbrotRow : Dict Point Int -> Int -> Int -> Html Msg
viewMandelbrotRow pixels width currentHeight =
    div
        [ style "height" "1px"
        ]
        (List.range 0 width
            |> List.map (viewPixel pixels currentHeight)
        )


viewPixel : Dict Point Int -> Int -> Int -> Html Msg
viewPixel pixels currentHeight currentWidth =
    let
        colorShade itr =
            1 - toFloat itr / 50

        rgbColor =
            case Dict.get ( currentWidth, currentHeight ) pixels of
                Nothing ->
                    "black"

                Just iterations ->
                    rgb 0.0 0.0 (colorShade iterations)
                        |> toCssString
    in
    div
        [ style "width" "1px"
        , style "height" "1px"

        -- , style "background-color" color
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
        maxMandelbrotIterations

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
            (xMax - xMin) / toFloat width

        yFactor =
            (yMax - yMin) / toFloat height

        calculateRow row mdict =
            List.range 0 width
                |> List.foldl (calculateMandelbrotPixel xFactor yFactor row) mdict

        rows =
            List.range 0 height
                |> List.foldl calculateRow Dict.empty
    in
    rows


xMin =
    -2.0


xMax =
    1.0


yMin =
    -2.0


yMax =
    2.0


maxMandelbrotIterations =
    50


zeroComplex =
    { r = 0, i = 0 }


calculateMandelbrotPixel : Float -> Float -> Int -> Int -> Dict Point Int -> Dict Point Int
calculateMandelbrotPixel xFactor yFactor y x pixels =
    let
        iterations =
            iterateComplexMandelbrot (toComplex xFactor yFactor y x) (toComplex xFactor yFactor y x) maxMandelbrotIterations 0
    in
    Dict.insert ( x, y ) iterations pixels


toComplex : Float -> Float -> Int -> Int -> ComplexNumber
toComplex xFactor yFactor y x =
    let
        r =
            toFloat x * xFactor + xMin

        i =
            toFloat y * yFactor + yMin
    in
    { r = r, i = i }
