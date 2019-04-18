module Sketches.Example1 exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Dom as Dom
import Browser.Events exposing (onAnimationFrameDelta, onMouseMove, onResize)
import Html exposing (Html, div, h1, text)
import Html.Attributes exposing (id)
import Json.Decode as Decode
import Shared exposing (..)
import Task


type alias Model =
    SharedModel
        { counter : Int
        , position : Position
        , element : Maybe Dom.Element
        , error : Maybe String
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
            ( { model | counter = round diff }, Cmd.none )

        OnMouseMove x y ->
            ( { model | position = { x = x, y = y } }, Cmd.none )

        OnElement element ->
            ( { model | element = Just element }, Cmd.none )

        OnError error ->
            ( { model | error = Just error }, Cmd.none )

        OnWindowResize x y ->
            ( model, getPosition )


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
    div []
        [ case model.error of
            Nothing ->
                viewValid model

            Just error ->
                viewError error
        ]


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
                elementX =
                    model.position.x - element.element.x

                elementY =
                    model.position.y - element.element.y

                eW =
                    element.element.width

                eH =
                    element.element.height
            in
            div []
                [ h1 [] [ text <| String.fromFloat elementX ]
                , h1 [] [ text <| String.fromFloat elementY ]
                , h1 [] [ text <| String.fromFloat eW ]
                , h1 [] [ text <| String.fromFloat eH ]
                ]

        Nothing ->
            div []
                [ h1 [] [ text "Element position not ready" ]
                ]
