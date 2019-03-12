module Sketches.Sketch1 exposing (Model, Msg, init, update, view)

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


add2 a b =
    a + b


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Html Msg
view model =
    h1 [] [ text "Test Sketch 1" ]
