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
        , position : Position
        , element : Maybe Dom.Element
        , error : Maybe String
        , mouseTrail : List Position
        }


type alias Position =
    { x : Float
    , y : Float
    }


type Msg
    = NoOp
    | OnAnimationFrameDelta Float
    | OnMouseMove Float Float
    | OnElement Dom.Element
    | OnWindowResize Int Int
    | OnError String
    | OnKeyChange Direction


type Direction
    = Left
    | Right
    | Other


init : ( Model, Cmd Msg )
init =
    let
        info =
            { title = "Example 1 - Mouse trail"
            , markdown = """
Move the mouse around in the window to play with the mouse trail.

Press right/left arrow key to change the number of items in the mouse trail.

The window position and size is also tracked by using Browser.Events
            """
            }
    in
    ( { info = info
      , mouseTrailItemLength = 20
      , position = { x = 0, y = 0 }
      , element = Nothing
      , error = Nothing
      , mouseTrail = []
      }
    , getPosition
    )


getPosition : Cmd Msg
getPosition =
    let
        processElement e =
            case e of
                Ok result ->
                    OnElement result

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
            ( { model | position = { x = x, y = y } }, Cmd.none )

        OnElement element ->
            ( { model | element = Just element }, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        OnWindowResize x y ->
            ( model, getPosition )

        OnKeyChange direction ->
            let
                minItems =
                    5

                maxItems =
                    100

                items =
                    case direction of
                        Left ->
                            if model.mouseTrailItemLength > minItems then
                                model.mouseTrailItemLength - 1

                            else
                                model.mouseTrailItemLength

                        Right ->
                            if model.mouseTrailItemLength < maxItems then
                                model.mouseTrailItemLength + 1

                            else
                                model.mouseTrailItemLength

                        Other ->
                            model.mouseTrailItemLength
            in
            ( { model | mouseTrailItemLength = items }, Cmd.none )


addPositionToTrail : Model -> List Position
addPositionToTrail model =
    case model.element of
        Just element ->
            if (model.position.x >= element.element.x && model.position.x <= element.element.x + element.element.width) && (model.position.y >= element.element.y && model.position.y <= element.element.y + element.element.height) then
                let
                    translatePosition x y position =
                        { x = position.x - x, y = position.y - y }
                in
                translatePosition element.element.x element.element.y model.position
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
                , viewMousePosition model
                ]

        Just error ->
            div [ class "sketch-default-container" ]
                [ viewError error
                ]


viewMouseTrail : Model -> Html Msg
viewMouseTrail model =
    div [ class "sketch-default-main-item", id "sketch-drawing-area" ]
        (case model.element of
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
viewTrailItem position =
    let
        pX =
            position.x
                |> String.fromFloat

        pY =
            position.y
                |> String.fromFloat
    in
    circle [ cx pX, cy pY, r "10", fill "blue" ] []


viewValid : Model -> Html Msg
viewValid model =
    div []
        [ viewMousePosition model
        ]


viewError : String -> Html Msg
viewError error =
    div []
        [ h1 [] [ text error ]
        ]


viewMousePosition : Model -> Html Msg
viewMousePosition model =
    case model.element of
        Just element ->
            let
                mouseTrailItemLength =
                    model.mouseTrailItemLength
                        |> String.fromInt
                        |> (\n -> "Trail items: " ++ n)

                mouseX =
                    model.position.x
                        |> round
                        |> String.fromInt
                        |> (\n -> "Mouse X: " ++ n)

                mouseY =
                    model.position.y
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
                [ h1 [] [ text "Element position not ready" ]
                ]
