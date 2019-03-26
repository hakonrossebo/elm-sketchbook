module SketchNavigation exposing (Route(..), SketchItem(..), allMenus, examplesMenu, menu, parseUrl, viewMenu, viewMenus)

import Array
import Html exposing (Html, a, div, h1, h2, h3, h4, img, li, text, ul)
import Html.Attributes exposing (class, href, src)
import Url exposing (Url)
import Url.Parser exposing (..)


type SketchItem
    = SketchItem1
    | SketchItem2


type Route
    = SketchRoute Int
    | ExampleRoute Int
    | GettingStartedRoute
    | NotFoundRoute


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map GettingStartedRoute top
        , map SketchRoute (s "sketches" </> int)
        , map ExampleRoute (s "examples" </> int)
        ]


parseUrl : Url -> Route
parseUrl url =
    case parse matchers url of
        Just route ->
            route

        Nothing ->
            NotFoundRoute


type SketchMenuInfo
    = SketchMenuContainer String
    | SketchMenuItem String Route


type MenuItem
    = MenuNode SketchMenuInfo (List MenuItem)


type alias MenuItemList =
    List MenuItem


allMenus : MenuItemList
allMenus =
    [ menu, examplesMenu ]


menu =
    MenuNode
        (SketchMenuContainer
            "Sketches"
        )
        [ MenuNode (SketchMenuItem "Sketch 1" (SketchRoute 1)) []
        , MenuNode (SketchMenuItem "Sketch 2" (SketchRoute 2)) []
        , MenuNode (SketchMenuItem "Sketch 3" (SketchRoute 3)) []
        , MenuNode (SketchMenuItem "Sketch 4" (SketchRoute 4)) []
        ]


examplesMenu =
    MenuNode
        (SketchMenuContainer
            "Examples"
        )
        [ MenuNode (SketchMenuItem "Example 1" (ExampleRoute 1)) []
        , MenuNode (SketchMenuItem "Example 2" (ExampleRoute 2)) []
        , MenuNode (SketchMenuItem "Example 3" (ExampleRoute 3)) []
        ]


getMenuContainerItemLength : MenuItem -> Int
getMenuContainerItemLength (MenuNode _ items) =
    List.length items


viewMenus : MenuItemList -> Html msg
viewMenus menuItems =
    div []
        (menuItems
            |> List.map viewMenu
        )


viewMenu : MenuItem -> Html msg
viewMenu (MenuNode info items) =
    let
        subChapters =
            List.map viewMenu items
    in
    case info of
        SketchMenuContainer title ->
            ul []
                [ li []
                    [ h3 [] [ text title ]
                    ]
                , ul [] subChapters
                ]

        SketchMenuItem title route ->
            ul []
                [ li []
                    [ h4 [] [ a [ href (pathFor route) ] [ text title ] ]
                    ]
                , ul [] subChapters
                ]


pathFor : Route -> String
pathFor route =
    case route of
        SketchRoute id ->
            "/sketches/" ++ String.fromInt id

        ExampleRoute id ->
            "/examples/" ++ String.fromInt id

        GettingStartedRoute ->
            "/"

        NotFoundRoute ->
            "/notfound"
