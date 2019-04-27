module Sketches.Sketch3 exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, h1, text)
import Shared exposing (..)


type alias Model =
    SharedModel
        { counter : Int
        }


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    let
        info =
            { title = "Sketch Title"
            , markdown = "Sketch markdown"
            }
    in
    ( { counter = 0, info = info }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    h1 [] [ text "Welcome to Elm Sketchbook. Start extending this empty sketch, or use some of the examples as a starting point." ]
