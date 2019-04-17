module SketchManager exposing (Model, Msg(..), init, loadCurrentSketch, subscriptions, update, view, viewSketchInformation)

import Browser
import Html exposing (Html, div, h1, h2, img, text)
import Html.Attributes exposing (class, src)
import Markdown exposing (..)
import Shared exposing (..)
import SketchNavigation as Nav exposing (..)
import Sketches.Example1 as Example1
import Sketches.Example2 as Example2
import Sketches.Example3 as Example3
import Sketches.GettingStarted as GettingStarted
import Sketches.NotFound as NotFound
import Sketches.Sketch1 as Sketch1
import Sketches.Sketch2 as Sketch2
import Sketches.Sketch3 as Sketch3
import Sketches.Sketch4 as Sketch4



---- MODEL ----


type alias Model =
    SharedModel
        { sketch : Sketch
        , info : CommonSketchInformation
        }


type Sketch
    = NoSketch
    | Sketch1Model Sketch1.Model
    | Sketch2Model Sketch2.Model
    | Sketch3Model Sketch3.Model
    | Sketch4Model Sketch4.Model
    | Example1Model Sketch1.Model
    | Example2Model Sketch2.Model
    | Example3Model Sketch3.Model
    | GettingStartedModel GettingStarted.Model
    | NotFoundModel NotFound.Model


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmd ) =
            GettingStarted.init
    in
    ( { sketch = GettingStartedModel model, info = model.info }, Cmd.none )


mapModel : (SharedModel a -> Sketch) -> (b -> Msg) -> ( SharedModel a, Cmd b ) -> ( Model, Cmd Msg )
mapModel sketchModel sketchMsg ( model, msg ) =
    ( { sketch = sketchModel model, info = model.info }, Cmd.map sketchMsg msg )


initSketch : Int -> ( Model, Cmd Msg )
initSketch sketchId =
    case sketchId of
        1 ->
            mapModel Sketch1Model Sketch1Msg Sketch1.init

        2 ->
            mapModel Sketch2Model Sketch2Msg Sketch2.init

        3 ->
            mapModel Sketch3Model Sketch3Msg Sketch3.init

        4 ->
            mapModel Sketch4Model Sketch4Msg Sketch4.init

        _ ->
            initNotFound


initExample : Int -> ( Model, Cmd Msg )
initExample id =
    case id of
        1 ->
            mapModel Example1Model Example1Msg Example1.init

        2 ->
            mapModel Example2Model Example2Msg Example2.init

        3 ->
            mapModel Example3Model Example3Msg Example3.init

        _ ->
            initNotFound


initGettingStarted : ( Model, Cmd Msg )
initGettingStarted =
    let
        ( model, cmd ) =
            GettingStarted.init
    in
    ( { sketch = GettingStartedModel model, info = model.info }, Cmd.none )


initNotFound : ( Model, Cmd Msg )
initNotFound =
    let
        ( model, cmd ) =
            NotFound.init
    in
    ( { sketch = NotFoundModel model, info = model.info }, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp
    | Sketch1Msg Sketch1.Msg
    | Sketch2Msg Sketch2.Msg
    | Sketch3Msg Sketch3.Msg
    | Sketch4Msg Sketch4.Msg
    | Example1Msg Example1.Msg
    | Example2Msg Example2.Msg
    | Example3Msg Example3.Msg
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
                    initSketch sketchId

                Nav.ExampleRoute exampleId ->
                    initExample exampleId

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
            ( { model | sketch = Sketch1Model newSketchModel }, Cmd.map Sketch1Msg newSketchCmd )

        ( Sketch2Msg subMsg, Sketch2Model sketchModel ) ->
            let
                ( newSketchModel, newSketchCmd ) =
                    Sketch2.update subMsg sketchModel
            in
            ( { model | sketch = Sketch2Model newSketchModel }, Cmd.map Sketch2Msg newSketchCmd )

        ( Sketch3Msg subMsg, Sketch3Model sketchModel ) ->
            let
                ( newSketchModel, newSketchCmd ) =
                    Sketch3.update subMsg sketchModel
            in
            ( { model | sketch = Sketch3Model newSketchModel }, Cmd.map Sketch3Msg newSketchCmd )

        ( Sketch4Msg subMsg, Sketch4Model sketchModel ) ->
            let
                ( newSketchModel, newSketchCmd ) =
                    Sketch4.update subMsg sketchModel
            in
            ( { model | sketch = Sketch4Model newSketchModel }, Cmd.map Sketch4Msg newSketchCmd )

        ( Example1Msg subMsg, Example1Model exampleModel ) ->
            let
                ( newExampleModel, newExampleCmd ) =
                    Example1.update subMsg exampleModel
            in
            ( { model | sketch = Example1Model newExampleModel }, Cmd.map Example1Msg newExampleCmd )

        ( Example2Msg subMsg, Example2Model exampleModel ) ->
            let
                ( newExampleModel, newExampleCmd ) =
                    Example2.update subMsg exampleModel
            in
            ( { model | sketch = Example2Model newExampleModel }, Cmd.map Example2Msg newExampleCmd )

        ( Example3Msg subMsg, Example3Model exampleModel ) ->
            let
                ( newExampleModel, newExampleCmd ) =
                    Example3.update subMsg exampleModel
            in
            ( { model | sketch = Example3Model newExampleModel }, Cmd.map Example3Msg newExampleCmd )

        ( GettingStartedMsg subMsg, GettingStartedModel gettingStartedModel ) ->
            let
                ( newGettingStartedModel, newGettingStartedCmd ) =
                    GettingStarted.update subMsg gettingStartedModel
            in
            ( { model | sketch = GettingStartedModel newGettingStartedModel }, Cmd.map GettingStartedMsg newGettingStartedCmd )

        ( NotFoundMsg subMsg, NotFoundModel notFoundModel ) ->
            let
                ( newNotFoundModel, newNotFoundCmd ) =
                    NotFound.update subMsg notFoundModel
            in
            ( { model | sketch = NotFoundModel notFoundModel }, Cmd.map NotFoundMsg newNotFoundCmd )

        ( Sketch1Msg subMsg, _ ) ->
            ( model, Cmd.none )

        ( Sketch2Msg subMsg, _ ) ->
            ( model, Cmd.none )

        ( Sketch3Msg subMsg, _ ) ->
            ( model, Cmd.none )

        ( Sketch4Msg subMsg, _ ) ->
            ( model, Cmd.none )

        ( Example1Msg subMsg, _ ) ->
            ( model, Cmd.none )

        ( Example2Msg subMsg, _ ) ->
            ( model, Cmd.none )

        ( Example3Msg subMsg, _ ) ->
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

            Sketch3Model sketchModel ->
                Sketch3.view sketchModel
                    |> Html.map Sketch3Msg

            Sketch4Model sketchModel ->
                Sketch4.view sketchModel
                    |> Html.map Sketch4Msg

            Example1Model exampleModel ->
                Example1.view exampleModel
                    |> Html.map Example1Msg

            Example2Model exampleModel ->
                Example2.view exampleModel
                    |> Html.map Example2Msg

            Example3Model exampleModel ->
                Example3.view exampleModel
                    |> Html.map Example3Msg

            NotFoundModel notFoundModel ->
                NotFound.view notFoundModel
                    |> Html.map NotFoundMsg

            GettingStartedModel gettingStartedModel ->
                GettingStarted.view gettingStartedModel
                    |> Html.map GettingStartedMsg
        ]


viewSketchInformation : Model -> Html Msg
viewSketchInformation model =
    div [ class "rightSideArea" ]
        [ h2 [] [ text model.info.title ]
        , toHtml [] model.info.markdown
        ]



---- PROGRAM ----


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.sketch of
        Sketch1Model sketchModel ->
            Sub.map Sketch1Msg (Sketch1.subscriptions sketchModel)

        Sketch2Model sketchModel ->
            Sub.map Sketch2Msg (Sketch2.subscriptions sketchModel)

        Sketch3Model sketchModel ->
            Sub.map Sketch3Msg (Sketch3.subscriptions sketchModel)

        Sketch4Model sketchModel ->
            Sub.map Sketch4Msg (Sketch4.subscriptions sketchModel)

        Example1Model exampleModel ->
            Sub.map Example1Msg (Example1.subscriptions exampleModel)

        Example2Model exampleModel ->
            Sub.map Example2Msg (Example2.subscriptions exampleModel)

        Example3Model exampleModel ->
            Sub.map Example3Msg (Example3.subscriptions exampleModel)

        GettingStartedModel gettingStartedModel ->
            Sub.map GettingStartedMsg (GettingStarted.subscriptions gettingStartedModel)

        NotFoundModel notFoundModel ->
            Sub.map NotFoundMsg (NotFound.subscriptions notFoundModel)

        NoSketch ->
            Sub.none
