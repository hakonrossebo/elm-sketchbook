module Sketches.Example3 exposing (Model, Msg, init, subscriptions, update, view)

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
            { title = "E3 Title"
            , markdown = "E3 markdown"
            }
    in
    ( { counter = 0, info = info }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( { model | counter = 0 }, Cmd.none )


add2 a b =
    a + b


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    h1 [] [ text "Test Example 3" ]
