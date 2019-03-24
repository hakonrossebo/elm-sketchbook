module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (Html, div, h1, h2, h3, h4, img, text)
import Html.Attributes exposing (class, src)
import SketchManager
import SketchNavigation as Nav exposing (..)



---- MODEL ----


type alias Model =
    { sketchModel : SketchManager.Model }


init : ( Model, Cmd Msg )
init =
    let
        ( model, cmd ) =
            SketchManager.init
    in
    ( { sketchModel = model }, Cmd.map SketchMsg cmd )



---- UPDATE ----


type Msg
    = NoOp
    | SketchMsg SketchManager.Msg


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
            ( { sketchModel = newSketchModel }, Cmd.map SketchMsg newSketchCmd )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ -- img [ src "/logo.svg" ] []
          div [ class "pageHeader" ] [ h2 [] [ text "Elm Sketchbook" ] ]

        -- , div [ class "mainNav" ] [ text "Nav text" ]
        -- , div [ class "mainNav" ] [ Nav.viewToC Nav.chapters ]
        , div [ class "mainNav" ]
            -- [ Nav.viewMenu Nav.menu
            -- , Nav.viewMenu Nav.examplesMenu
            -- ]
            [ Nav.viewMenus allMenus
            ]

        -- , h1 [] [ text "Your Elm App is working!" ]
        -- , div []
        -- [
        , SketchManager.view model.sketchModel
            |> Html.map SketchMsg
        , div [ class "rightSideArea" ] [ text "Right side  text" ]
        , div [ class "pageFooter" ] [ text "Footer  text" ]

        -- ]
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SketchMsg (SketchManager.subscriptions model.sketchModel)
