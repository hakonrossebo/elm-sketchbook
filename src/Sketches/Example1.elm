module Sketches.Example1 exposing (Model, Msg, init, subscriptions, update, view)

import Browser.Events exposing (onAnimationFrameDelta)
import Html exposing (Html, h1, text)
import Shared exposing (..)


type alias Model =
    SharedModel
        { counter : Int
        }


type Msg
    = NoOp
    | OnAnimationFrameDelta Float


init : ( Model, Cmd Msg )
init =
    let
        info =
            { title = "E1 Title"
            , markdown = "E1 markdown"
            }
    in
    ( { counter = 0, info = info }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnAnimationFrameDelta diff ->
            ( { model | counter = round diff }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta OnAnimationFrameDelta
        ]


view : Model -> Html Msg
view model =
    h1 [] [ text <| String.fromInt model.counter ]
