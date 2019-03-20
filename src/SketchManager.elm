module SketchManager exposing (Model, Msg(..), init, subscriptions, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, src)
import Sketches.Sketch1 as Sketch1
import Sketches.Sketch2 as Sketch2



---- MODEL ----


type alias Model =
    { sketch : Sketch }


type Sketch
    = NoSketch
    | Sketch1Model Sketch1.Model
    | Sketch2Model Sketch2.Model


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmd ) =
            Sketch1.init
    in
    ( { sketch = Sketch1Model model }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Sketch1Msg Sketch1.Msg
    | Sketch2Msg Sketch2.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.sketch ) of
        ( NoOp, _ ) ->
            ( model, Cmd.none )

        ( Sketch1Msg subMsg, Sketch1Model sketchModel ) ->
            let
                ( newSketchModel, newSketchCmd ) =
                    Sketch1.update subMsg sketchModel
            in
            ( model, Cmd.map Sketch1Msg newSketchCmd )

        ( Sketch2Msg subMsg, Sketch2Model sketchModel ) ->
            let
                ( newSketchModel, newSketchCmd ) =
                    Sketch2.update subMsg sketchModel
            in
            ( model, Cmd.map Sketch2Msg newSketchCmd )

        ( Sketch1Msg subMsg, _ ) ->
            ( model, Cmd.none )

        ( Sketch2Msg subMsg, _ ) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "sketchArea" ]
        [ case model.sketch of
            NoSketch ->
                div [] [ h1 [] [ text "no sketch selected" ] ]

            Sketch1Model sketchModel ->
                Sketch1.view sketchModel
                    |> Html.map Sketch1Msg

            Sketch2Model sketchModel ->
                Sketch2.view sketchModel
                    |> Html.map Sketch2Msg
        ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.sketch of
        Sketch1Model sketchModel ->
            Sub.map Sketch1Msg (Sketch1.subscriptions sketchModel)

        Sketch2Model sketchModel ->
            Sub.map Sketch2Msg (Sketch2.subscriptions sketchModel)

        NoSketch ->
            Sub.none
