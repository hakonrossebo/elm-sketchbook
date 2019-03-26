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



-- type TreeToC
--     = Item String SketchItem
--     | Chapter String (List TreeToC)
-- chapters =
--     Chapter "Sketches"
--         [ Item "Sketch 1" SketchItem1
--         , Item "Sketch 2" SketchItem2
--         , Item "Sketch 3" SketchItem2
--         ]
-- chapters =
--     Chapter "Sketches"
--         [ Chapter "Nested 1"
--             [ Item "Sketch item 1" SketchItem1
--             , Item "Sketch item 2" SketchItem2
--             ]
--         ]


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
            "Sketches M"
        )
        [ MenuNode (SketchMenuItem "Sketch 1" (SketchRoute 1)) []
        , MenuNode (SketchMenuItem "Sketch 2" (SketchRoute 2)) []
        , MenuNode (SketchMenuItem "Sketch 3" (SketchRoute 2)) []
        ]


examplesMenu =
    MenuNode
        (SketchMenuContainer
            "Examples"
        )
        [ MenuNode (SketchMenuItem "Example 1" (ExampleRoute 1)) []
        , MenuNode (SketchMenuItem "Example 2" (ExampleRoute 1)) []
        , MenuNode (SketchMenuItem "Example 3" (ExampleRoute 1)) []
        ]


getMenuContainerItemLength : MenuItem -> Int
getMenuContainerItemLength (MenuNode _ items) =
    List.length items



-- starte med en posisjon som gir navn og item
-- skal kunne klikke på et meny item og sette item som current model
-- Bruke navigation med url?
-- Frem og tilbake knapp for å kunne navigere til neste?
-- Vise valgt item i meny


viewMenus : MenuItemList -> Html msg
viewMenus menuItems =
    div []
        (menuItems
            |> List.map viewMenu
        )



-- View links should send sketch or example id
-- Need a count of sketches and examples - calculate from menu and examplesMenu?
-- Need a mapping from type and id to route and id that are valid


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
                    -- [ h4 [] [ text title ]
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



-- viewToC : TreeToC -> Html msg
-- viewToC tree =
--     case tree of
--         Item title item ->
--             li [] [ text title ]
--         Chapter title items ->
--             let
--                 subChapters =
--                     List.map viewToC items
--             in
--             ul []
--                 [ li []
--                     [ h3 [] [ text title ]
--                     ]
--                 , ul [] subChapters
--                 ]
