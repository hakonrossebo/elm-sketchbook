module Sketches.NotFound exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, h1, text)


type alias Model =
    { counter : Int
    }


type Msg
    = NoOp


init : ( Model, Cmd Msg )
init =
    ( { counter = 0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( { model | counter = 0 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    h1 [] [ text "Could not find the sketch for this route." ]
