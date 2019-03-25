module SketchManager exposing (Model, Msg(..), init, loadCurrentSketch, subscriptions, update, view)

import Browser
import Html exposing (Html, div, h1, img, text)
import Html.Attributes exposing (class, src)
import SketchNavigation as Nav exposing (..)
import Sketches.Example1 as Example1
import Sketches.GettingStarted as GettingStarted
import Sketches.NotFound as NotFound
import Sketches.Sketch1 as Sketch1
import Sketches.Sketch2 as Sketch2



---- MODEL ----


type alias Model =
    { sketch : Sketch }


type Sketch
    = NoSketch
    | Sketch1Model Sketch1.Model
    | Sketch2Model Sketch2.Model
    | Example1Model Sketch1.Model
    | GettingStartedModel GettingStarted.Model
    | NotFoundModel NotFound.Model


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmd ) =
            Sketch1.init
    in
    ( { sketch = Sketch1Model model }, Cmd.none )


initGettingStarted : ( Model, Cmd Msg )
initGettingStarted =
    let
        ( model, cmd ) =
            GettingStarted.init
    in
    ( { sketch = GettingStartedModel model }, Cmd.none )


initNotFound : ( Model, Cmd Msg )
initNotFound =
    let
        ( model, cmd ) =
            NotFound.init
    in
    ( { sketch = NotFoundModel model }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Sketch1Msg Sketch1.Msg
    | Sketch2Msg Sketch2.Msg
    | Example1Msg Example1.Msg
    | GettingStartedMsg GettingStarted.Msg
    | NotFoundMsg NotFound.Msg


loadCurrentSketch : Nav.Route -> ( Model, Cmd Msg )
loadCurrentSketch route =
    let
        ( sketchModel, newCmd ) =
            case route of
                Nav.GettingStartedRoute ->
                    initGettingStarted

                Nav.SketchRoute sketchId ->
                    init

                Nav.ExampleRoute exampleId ->
                    init

                Nav.NotFoundRoute ->
                    initNotFound
    in
    ( sketchModel, newCmd )


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

        ( Example1Msg subMsg, Example1Model exampleModel ) ->
            let
                ( newExampleModel, newExampleCmd ) =
                    Example1.update subMsg exampleModel
            in
            ( model, Cmd.map Example1Msg newExampleCmd )

        ( GettingStartedMsg subMsg, GettingStartedModel gettingStartedModel ) ->
            let
                ( newGettingStartedModel, newGettingStartedCmd ) =
                    GettingStarted.update subMsg gettingStartedModel
            in
            ( model, Cmd.map GettingStartedMsg newGettingStartedCmd )

        ( NotFoundMsg subMsg, NotFoundModel notFoundModel ) ->
            let
                ( newNotFoundModel, newNotFoundCmd ) =
                    NotFound.update subMsg notFoundModel
            in
            ( model, Cmd.map NotFoundMsg newNotFoundCmd )

        ( Sketch1Msg subMsg, _ ) ->
            ( model, Cmd.none )

        ( Sketch2Msg subMsg, _ ) ->
            ( model, Cmd.none )

        ( Example1Msg subMsg, _ ) ->
            ( model, Cmd.none )

        ( GettingStartedMsg subMsg, _ ) ->
            ( model, Cmd.none )

        ( NotFoundMsg subMsg, _ ) ->
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

            Example1Model exampleModel ->
                Example1.view exampleModel
                    |> Html.map Example1Msg

            NotFoundModel notFoundModel ->
                NotFound.view notFoundModel
                    |> Html.map NotFoundMsg

            GettingStartedModel gettingStartedModel ->
                GettingStarted.view gettingStartedModel
                    |> Html.map GettingStartedMsg
        ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.sketch of
        Sketch1Model sketchModel ->
            Sub.map Sketch1Msg (Sketch1.subscriptions sketchModel)

        Sketch2Model sketchModel ->
            Sub.map Sketch2Msg (Sketch2.subscriptions sketchModel)

        Example1Model exampleModel ->
            Sub.map Example1Msg (Example1.subscriptions exampleModel)

        GettingStartedModel gettingStartedModel ->
            Sub.map GettingStartedMsg (GettingStarted.subscriptions gettingStartedModel)

        NotFoundModel notFoundModel ->
            Sub.map NotFoundMsg (NotFound.subscriptions notFoundModel)

        NoSketch ->
            Sub.none
