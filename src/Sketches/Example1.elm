module Sketches.Example1 exposing (Model, Msg, init, subscriptions, update, view)

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
        { mouseTrailItemLength : Int
        , mousePosition : Position
        , sketchDrawingArea : Maybe Dom.Element
        , error : Maybe String
        , mouseTrail : List Position
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
            { title = "Example 1 - Mouse trail"
            , markdown = """
Move the mouse around in the window to play with the mouse trail.

Use arrow keys to change the number of items in the mouse trail.

The window mousePosition and size is also tracked by using Browser.Events
            """
            }
    in
    ( { info = info
      , mouseTrailItemLength = 20
      , mousePosition = { x = 0, y = 0 }
      , sketchDrawingArea = Nothing
      , error = Nothing
      , mouseTrail = []
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
            let
                trail =
                    addPositionToTrail model
            in
            ( { model | mouseTrail = trail }, Cmd.none )

        OnMouseMove x y ->
            ( { model | mousePosition = { x = x, y = y } }, Cmd.none )

        OnSketchDrawingAreaFound element ->
            ( { model | sketchDrawingArea = Just element }, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        OnWindowResize x y ->
            ( model, getSketchDrawingArea )

        OnKeyChange direction ->
            ( { model | mouseTrailItemLength = updateMouseTrailItemLength direction model.mouseTrailItemLength }, Cmd.none )


updateMouseTrailItemLength : Direction -> Int -> Int
updateMouseTrailItemLength direction mouseTrailItemLength =
    let
        minItems =
            5

        maxItems =
            100

        items =
            case direction of
                Left ->
                    if mouseTrailItemLength > minItems then
                        mouseTrailItemLength - 1

                    else
                        mouseTrailItemLength

                Down ->
                    if mouseTrailItemLength > minItems then
                        mouseTrailItemLength - 1

                    else
                        mouseTrailItemLength

                Right ->
                    if mouseTrailItemLength < maxItems then
                        mouseTrailItemLength + 1

                    else
                        mouseTrailItemLength

                Up ->
                    if mouseTrailItemLength < maxItems then
                        mouseTrailItemLength + 1

                    else
                        mouseTrailItemLength

                Other ->
                    mouseTrailItemLength
    in
    items


addPositionToTrail : Model -> List Position
addPositionToTrail model =
    case model.sketchDrawingArea of
        Just element ->
            if (model.mousePosition.x >= element.element.x && model.mousePosition.x <= element.element.x + element.element.width) && (model.mousePosition.y >= element.element.y && model.mousePosition.y <= element.element.y + element.element.height) then
                let
                    translatePosition x y mousePosition =
                        { x = mousePosition.x - x, y = mousePosition.y - y }
                in
                translatePosition element.element.x element.element.y model.mousePosition
                    :: model.mouseTrail
                    |> List.take model.mouseTrailItemLength

            else
                []

        Nothing ->
            []


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
                [ div [ class "sketch-default-top-item" ] [ text "Mouse - Keyboard and Window management" ]
                , viewMouseTrail model
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


viewMouseTrail : Model -> Html Msg
viewMouseTrail model =
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
                    (model.mouseTrail
                        |> List.map viewTrailItem
                    )
                ]

            Nothing ->
                [ text "Drawing area not ready"
                ]
        )


viewTrailItem : Position -> Html Msg
viewTrailItem mousePosition =
    let
        pX =
            mousePosition.x
                |> String.fromFloat

        pY =
            mousePosition.y
                |> String.fromFloat
    in
    circle [ cx pX, cy pY, r "10", fill "blue" ] []


viewMousePositionInformation : Model -> Html Msg
viewMousePositionInformation model =
    case model.sketchDrawingArea of
        Just element ->
            let
                mouseTrailItemLength =
                    model.mouseTrailItemLength
                        |> String.fromInt
                        |> (\n -> "Trail items: " ++ n)

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
                [ span [] [ text mouseTrailItemLength ]
                , span [] [ text mouseX ]
                , span [] [ text mouseY ]
                , span [] [ text windowWidth ]
                , span [] [ text windowHeight ]
                ]

        Nothing ->
            div [ class "sketch-default-footer-item" ]
                [ h1 [] [ text "Sketch drawing area not ready" ]
                ]
