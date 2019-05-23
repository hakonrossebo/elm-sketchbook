module Sketches.Example4 exposing (Model, Msg, addComplex, init, iterateComplexMandelbrot, magnitudeComplex, subscriptions, update, view)

import Browser.Dom as Dom
import Browser.Events exposing (onAnimationFrameDelta, onKeyDown, onMouseMove, onResize)
import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class, id)
import Json.Decode as Decode
import Shared exposing (..)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, viewBox, width)
import Task


type alias Model =
    SharedModel
        { mousePosition : Position
        , sketchDrawingArea : Maybe Dom.Element
        , pixels : List (List Int)
        , error : Maybe String
        }


type alias Position =
    { x : Float
    , y : Float
    }


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
      , pixels = [ [] ]
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
            in
            ( { model | sketchDrawingArea = Just element, pixels = pixels }, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        OnWindowResize x y ->
            ( model, getSketchDrawingArea )

        OnKeyChange direction ->
            ( model, Cmd.none )


calculateMandelbrot : Int -> Int -> List (List Int)
calculateMandelbrot width height =
    let
        calculateRow row =
            List.range 0 width
                |> List.map (calculateMandelbrotPixel width height row)

        rows =
            List.range 0 height
                |> List.map calculateRow
    in
    rows


zeroComplex =
    { r = 0, i = 0 }


calculateMandelbrotPixel : Int -> Int -> Int -> Int -> Int
calculateMandelbrotPixel width height y x =
    iterateComplexMandelbrot zeroComplex (toComplex width height y x) 50 0


toComplex : Int -> Int -> Int -> Int -> ComplexNumber
toComplex width height y x =
    let
        r =
            (toFloat x - toFloat width / 2)
                / 4

        i =
            (toFloat y - toFloat height / 2)
                / 4
    in
    { r = r, i = i }


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
                            |> String.fromInt

                    windowHeight =
                        element.element.height
                            |> round
                            |> String.fromInt

                    viewBoxInfo =
                        "0 0 " ++ windowWidth ++ " " ++ windowHeight
                in
                [ svg
                    [ width windowWidth
                    , height windowHeight
                    , viewBox viewBoxInfo
                    ]
                    [ circle [ cx "100", cy "100", r "50", fill "red" ] []
                    , circle [ cx "200", cy "150", r "80", fill "blue" ] []
                    , circle [ cx "120", cy "170", r "30", fill "green" ] []
                    ]
                ]

            Nothing ->
                [ text "Drawing area not ready"
                ]
        )


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
        iterateComplexMandelbrot currentComplex newComplex maxIterations nextIteration
