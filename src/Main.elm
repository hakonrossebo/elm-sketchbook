module Main exposing (Model, Msg(..), init, main, update, view)

import Browser exposing (UrlRequest)
import Browser.Navigation as Nav exposing (Key)
import Html exposing (Html, div, h1, h2, h3, h4, img, text)
import Html.Attributes exposing (class, src)
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
    , sketchModel : SketchManager.Model
    }


init : Flags -> Url -> Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        ( model, cmd ) =
            SketchManager.init
    in
    ( { flags = flags
      , navKey = navKey
      , route = Nav.parseUrl url
      , sketchModel = model
      }
    , Cmd.map SketchMsg cmd
    )
        |> loadCurrentPage



---- UPDATE ----


type Msg
    = NoOp
    | SketchMsg SketchManager.Msg
    | UrlChanged Url
    | LinkClicked UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

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
            in
            ( { model | route = newRoute }
            , Cmd.none
            )
                |> loadCurrentPage


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
    { title = "App"
    , body = [ viewBody model ]
    }


viewBody : Model -> Html Msg
viewBody model =
    div [ class "container" ]
        [ div [ class "pageHeader" ] [ h2 [] [ text "Elm Sketchbook" ] ]
        , div [ class "mainNav" ]
            [ Nav.viewMenus allMenus
            ]
        , SketchManager.view model.sketchModel
            |> Html.map SketchMsg
        , div [ class "rightSideArea" ] [ text "Right side  text" ]
        , div [ class "pageFooter" ] [ text "Footer  text" ]
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
    Sub.map SketchMsg (SketchManager.subscriptions model.sketchModel)