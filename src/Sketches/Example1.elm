module Sketches.Example1 exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom as Dom
import Browser.Events exposing (onAnimationFrameDelta, onMouseMove, onResize)
import Html exposing (Html, div, h1, span, text)
import Html.Attributes exposing (class, id)
import Json.Decode as Decode
import Shared exposing (..)
import Svg exposing (circle, svg)
import Svg.Attributes exposing (cx, cy, fill, height, r, viewBox, width)
import Task


type alias Model =
    SharedModel
        { counter : Int
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


init : ( Model, Cmd Msg )
init =
    let
        info =
            { title = "E1 Title"
            , markdown = "E1 markdown"
            }
    in
    ( { info = info
      , counter = 0
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
    Dom.getElement "sketch-content"
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
            ( { model | counter = round diff, mouseTrail = trail }, Cmd.none )

        OnMouseMove x y ->
            ( { model | position = { x = x, y = y } }, Cmd.none )

        OnElement element ->
            ( { model | element = Just element }, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        OnWindowResize x y ->
            ( model, getPosition )


addPositionToTrail : Model -> List Position
addPositionToTrail model =
    case model.element of
        Just element ->
            if (model.position.x >= element.element.x && model.position.x <= element.element.x + element.element.width) && (model.position.y >= element.element.y && model.position.y <= element.element.y + element.element.height) then
                let
                    translatePosition x y position =
                        { x = position.x - x, y = position.y - y - 40 }
                in
                translatePosition element.element.x element.element.y model.position
                    :: model.mouseTrail
                    |> List.take 20

            else
                []

        Nothing ->
            []


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
        ]


view : Model -> Html Msg
view model =
    case model.error of
        Nothing ->
            div [ class "sketch-default-container", id "sketch-content" ]
                [ div [ class "sketch-default-top-item" ] [ text "Mouse - Keyboard and Window management" ]
                , viewMouseTrail model
                , viewMousePosition model
                ]

        -- viewValid model
        Just error ->
            div [ class "sketch-default-container", id "sketch-content" ]
                [ viewError error
                ]


viewMouseTrail : Model -> Html Msg
viewMouseTrail model =
    case model.element of
        Just element ->
            let
                windowWidth =
                    element.element.width
                        |> round
                        |> String.fromInt

                windowHeight =
                    element.element.height
                        |> round
                        |> (\n -> n - 80)
                        |> String.fromInt

                viewBoxInfo =
                    "0 0 " ++ windowWidth ++ " " ++ windowHeight
            in
            div [ class "sketch-default-main-item" ]
                [ svg
                    [ width windowWidth
                    , height windowHeight
                    , viewBox viewBoxInfo
                    ]
                    -- [ circle [ cx "30", cy "50", r "5", fill "blue" ] []
                    -- ]
                    (model.mouseTrail
                        |> List.map viewTrailItem
                    )
                ]

        Nothing ->
            div [ class "sketch-default-main-item" ]
                [ text "Window not ready"
                ]


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
                mouseX =
                    model.position.x
                        -- - element.element.x
                        |> String.fromFloat
                        |> (\n -> "Mouse X: " ++ n)

                mouseY =
                    model.position.y
                        -- - element.element.y
                        |> String.fromFloat
                        |> (\n -> "Mouse Y: " ++ n)

                windowWidth =
                    element.element.width
                        |> String.fromFloat
                        |> (\n -> "Window width: " ++ n)

                windowHeight =
                    element.element.height
                        |> String.fromFloat
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
                [ h1 [] [ text "Element position not ready" ]
                ]
