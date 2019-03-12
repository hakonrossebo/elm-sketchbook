module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (src)
import Sketches.Sketch1



---- MODEL ----


type alias Model =
    { sketch : Sketch }


type Sketch
    = NoSketch
    | Sketch1Model Sketches.Sketch1.Model


init : ( Model, Cmd Msg )
init =
    ( { sketch = NoSketch }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Sketch1Msg Sketches.Sketch1.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.sketch ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( Sketch1Msg subMsg, Sketch1Model sketchModel ) ->
            let
                ( newSketchModel, newSketchCmd ) =
                    Sketches.Sketch1.update subMsg sketchModel
            in
            ( model, Cmd.map Sketch1Msg newSketchCmd )

        ( Sketch1Msg subMsg, _ ) ->
            ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
