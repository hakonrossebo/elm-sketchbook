module Sketches.Example2 exposing (Model, Msg, init, subscriptions, update, view)

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
        , theta : Float
        , waveItems : List Position
        , thetaIncrement : Float
        , amplitude : Float
        , error : Maybe String
        , fps : Int
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


xSpacing =
    16


period =
    500


dx =
    (Basics.pi * 2) / period * xSpacing


init : ( Model, Cmd Msg )
init =
    let
        info =
            { title = "Sine wave"
            , markdown = """
Use the arrow keys to change theta and amplitude values.
            """
            }
    in
    ( { info = info
      , mousePosition = { x = 0, y = 0 }
      , sketchDrawingArea = Nothing
      , theta = 0
      , waveItems = []
      , thetaIncrement =
            0.02
      , amplitude =
            75.0
      , error = Nothing
      , fps = 0
      }
    , getSketchDrawingArea
    )


calculateWave : Float -> Float -> Float -> Float -> Float -> ( Float, List Position )
calculateWave width height theta amplitude thetaIncrement =
    let
        newTheta =
            theta + thetaIncrement

        length =
            width
                / xSpacing
                |> floor

        waveValues =
            List.range 0 length
                |> List.map (\yt -> dx * toFloat yt)
                |> List.map (\yt -> yt + newTheta)
                |> List.indexedMap (\idx yt -> { x = toFloat idx, y = sin yt * amplitude })
    in
    ( newTheta, waveValues )


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
            let
                ( newTheta, newWave ) =
                    case model.sketchDrawingArea of
                        Nothing ->
                            ( model.theta, model.waveItems )

                        Just element ->
                            calculateWave element.element.width element.element.height model.theta model.amplitude model.thetaIncrement

                newFps =
                    calculateAverageFps model.fps (1000.0 / diff)
            in
            ( { model | theta = newTheta, waveItems = newWave, fps = newFps }, Cmd.none )

        OnMouseMove x y ->
            ( { model | mousePosition = { x = x, y = y } }, Cmd.none )

        OnSketchDrawingAreaFound element ->
            ( { model | sketchDrawingArea = Just element }, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        OnWindowResize x y ->
            ( model, getSketchDrawingArea )

        OnKeyChange direction ->
            let
                ( newThetaIncrement, newAmplitude ) =
                    updateWaveParameters model direction
            in
            ( { model | amplitude = newAmplitude, thetaIncrement = newThetaIncrement }, Cmd.none )


updateWaveParameters : Model -> Direction -> ( Float, Float )
updateWaveParameters model direction =
    let
        minAmplitude =
            5

        maxAmplitude =
            190

        minThetaIncrement =
            0.0

        maxThetaIncrement =
            8.0

        dTheta =
            0.001

        dAmplitude =
            1

        ( newThetaIncrement, newAmplitude ) =
            case direction of
                Left ->
                    if model.thetaIncrement - dTheta >= minThetaIncrement then
                        ( model.thetaIncrement - dTheta, model.amplitude )

                    else
                        ( model.thetaIncrement, model.amplitude )

                Right ->
                    if model.thetaIncrement + dTheta <= maxThetaIncrement then
                        ( model.thetaIncrement + dTheta, model.amplitude )

                    else
                        ( model.thetaIncrement, model.amplitude )

                Up ->
                    if model.amplitude + dAmplitude <= maxAmplitude then
                        ( model.thetaIncrement, model.amplitude + dAmplitude )

                    else
                        ( model.thetaIncrement, model.amplitude )

                Down ->
                    if model.amplitude - dAmplitude >= minAmplitude then
                        ( model.thetaIncrement, model.amplitude - dAmplitude )

                    else
                        ( model.thetaIncrement, model.amplitude )

                _ ->
                    ( model.thetaIncrement, model.amplitude )
    in
    ( newThetaIncrement, newAmplitude )


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
                [ div [ class "sketch-default-top-item" ] [ text "Sine wave example" ]
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
                    (model.waveItems
                        |> List.map (viewWaveItem element.element.width element.element.height)
                    )
                ]

            Nothing ->
                [ text "Drawing area not ready"
                ]
        )


viewWaveItem : Float -> Float -> Position -> Html Msg
viewWaveItem width height position =
    let
        xPos =
            position.x
                * xSpacing
                |> round
                |> String.fromInt

        yPos =
            position.y
                + (height / 2)
                |> round
                |> String.fromInt
    in
    circle [ cx xPos, cy yPos, r "10", fill "green" ] []


viewMousePositionInformation : Model -> Html Msg
viewMousePositionInformation model =
    case model.sketchDrawingArea of
        Just element ->
            let
                amplitude =
                    model.amplitude
                        |> String.fromFloat
                        |> (\n -> "Amplitude: " ++ n)

                thetaIncrement =
                    model.thetaIncrement
                        |> String.fromFloat
                        |> (\n -> "Theta increment: " ++ n)

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

                fps =
                    model.fps
                        |> String.fromInt
                        |> (\n -> "FPS: " ++ n)
            in
            div [ class "sketch-default-footer-item" ]
                [ span [] [ text amplitude ]
                , span [] [ text thetaIncrement ]
                , span [] [ text windowWidth ]
                , span [] [ text windowHeight ]
                , span [] [ text fps ]
                ]

        Nothing ->
            div [ class "sketch-default-footer-item" ]
                [ h1 [] [ text "Sketch drawing area not ready" ]
                ]
