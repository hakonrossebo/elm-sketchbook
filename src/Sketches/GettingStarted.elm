module Sketches.GettingStarted exposing (Model, Msg, init, subscriptions, update, view)

import Html exposing (Html, h1, text)
import Html.Attributes exposing (class)
import Markdown exposing (toHtml)
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
            { title = "Getting started"
            , markdown = """
Features:

    * Menu Navigation
    * Next/Previous sketch navigation 
    * 10+ empty sketches to start with 
    * Many examples to learn from

How to use:

    * Start with Sketch1 and customize as you wish
    * Continue with other sketches as needed
    * Edit the menu to show only the sketches you want
    * Remove examples if needed
    """
            }
    in
    ( { counter = 0, info = info }, Cmd.none )


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
    -- h1 [] [ text "Getting started page" ]
    toHtml [ class "mainContent" ] markdown


markdown =
    """
Welcome to Elm-Sketchbook. Elm-Sketchbook was created when I found myself always needing to create and setup a new app when trying out something new.
With Elm-Sketchbook, you always have a new blank sketch ready that works as a separate app. All sketches can then be navigated with the menu.

    * Sketch out new ideas
    * Learn Elm
    * Learn from and expand on the the examples
    * Teachers and students - use this as a tool for teaching, learning and experimenting with Elm
    * Use as presentation for your package docs
    * Use it as a slide-show


The examples contain:

    * Basic Html in Elm
    * Use mouse and keyboard with graphics
    * Graphics programming examples
    * WebGL in Elm
    * Contribute other examples

Simplify the getting started experience by having features ready:
    * Commonly used packages and usage
    * Examples
    * Keyboard and mouse interaction/navigation
    * FPS display

"""
