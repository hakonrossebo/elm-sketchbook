module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (UrlRequest)
import Browser.Events exposing (onAnimationFrameDelta)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, a, div, h1, h2, h3, h4, i, img, span, text)
import Html.Attributes exposing (class, href, src, title)
import SketchManager
import SketchNavigation as Nav exposing (..)
import Url exposing (Url)



---- MODEL ----


type alias Flags =
    { api : String
    }


type alias Model =
    { flags : Flags
    , navKey : Key
    , route : Nav.Route
    , nextRoute : Maybe Nav.Route
    , previousRoute : Maybe Nav.Route
    , sketchModel : SketchManager.Model
    , sketchMenu : Nav.MenuItemList
    , fps : Int
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        ( sketchModel, sketchCmd ) =
            SketchManager.init

        route =
            Nav.parseUrl url

        menu =
            Nav.allMenus

        nextRoute =
            getNextItemInMenu route menu

        previousRoute =
            getPreviousItemInMenu route menu
    in
    ( { flags = flags
      , navKey = navKey
      , route = route
      , nextRoute = nextRoute
      , previousRoute = previousRoute
      , sketchModel = sketchModel
      , sketchMenu = menu
      , fps = 0
      }
    , Cmd.map SketchMsg sketchCmd
    )
        |> loadCurrentPage



---- UPDATE ----


type Msg
    = NoOp
    | SketchMsg SketchManager.Msg
    | UrlChanged Url
    | LinkClicked UrlRequest
    | OnAnimationFrameDelta Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        OnAnimationFrameDelta diff ->
            let
                newFps =
                    1000.0
                        / diff
            in
            ( { model | fps = calculateAverageFps model.fps newFps }, Cmd.none )

        SketchMsg sketchMsg ->
            let
                ( newSketchModel, newSketchCmd ) =
                    SketchManager.update sketchMsg model.sketchModel
            in
            ( { model | sketchModel = newSketchModel }, Cmd.map SketchMsg newSketchCmd )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.navKey (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            let
                newRoute =
                    Nav.parseUrl url

                nextRoute =
                    getNextItemInMenu newRoute model.sketchMenu

                previousRoute =
                    getPreviousItemInMenu newRoute model.sketchMenu
            in
            ( { model | route = newRoute, nextRoute = nextRoute, previousRoute = previousRoute }
            , Cmd.none
            )
                |> loadCurrentPage


calculateAverageFps : Int -> Float -> Int
calculateAverageFps old new =
    let
        smoothing =
            0.95

        floatOld =
            toFloat old

        avg =
            (floatOld * smoothing) + (new * (1.0 - smoothing))
    in
    round avg


loadCurrentPage : ( Model, Cmd Msg ) -> ( Model, Cmd Msg )
loadCurrentPage ( model, cmd ) =
    let
        ( sketchModel, newCmd ) =
            SketchManager.loadCurrentSketch model.route
    in
    ( { model | sketchModel = sketchModel }, Cmd.batch [ cmd, Cmd.map SketchMsg newCmd ] )



---- VIEW ----


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Sketchbook"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div [ class "container" ]
        [ div [ class "pageHeader" ] [ h2 [] [ text "Elm Sketchbook" ] ]
        , div [ class "mainNav" ]
            [ Nav.viewMenus model.route model.sketchMenu
            ]
        , SketchManager.view model.sketchModel
            |> Html.map SketchMsg
        , SketchManager.viewSketchInformation model.sketchModel
            |> Html.map SketchMsg
        , viewFooter model
        ]


viewFooter : Model -> Html Msg
viewFooter model =
    div [ class "pageFooter" ]
        [ viewNavigatePrevious model
        , viewNavigateNext model
        , text "FPS "
        , String.fromInt model.fps |> text
        ]


viewNavigateNext : Model -> Html Msg
viewNavigateNext model =
    case model.nextRoute of
        Just route ->
            span [ class "navbutton" ]
                [ a [ href (Nav.pathFor route) ] [ i [ title "Next", class "fas fa-long-arrow-alt-right fa-2x" ] [] ]
                ]

        Nothing ->
            span [ class "navbutton disabled" ]
                [ i [ class "fas fa-long-arrow-alt-right fa-2x" ] []
                ]


viewNavigatePrevious : Model -> Html Msg
viewNavigatePrevious model =
    case model.previousRoute of
        Just route ->
            span [ class "navbutton" ]
                [ a [ href (Nav.pathFor route) ] [ i [ title "Previous", class "fas fa-long-arrow-alt-left fa-2x" ] [] ]
                ]

        Nothing ->
            span [ class "navbutton disabled" ]
                [ i [ class "fas fa-long-arrow-alt-left fa-2x" ] []
                ]



---- PROGRAM ----


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onAnimationFrameDelta OnAnimationFrameDelta
        , Sub.map SketchMsg (SketchManager.subscriptions model.sketchModel)
        ]
