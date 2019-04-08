module SketchNavigation exposing (Route(..), allMenus, examplesMenu, getNextItemInMenu, menu, parseUrl, viewMenu, viewMenus)

import Array
import Html exposing (Html, a, div, h1, h2, h3, h4, img, li, text, ul)
import Html.Attributes exposing (class, href, src)
import Url exposing (Url)
import Url.Parser exposing (..)


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
    = MenuNode SketchMenuInfo MenuItemList


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



-- Keep model with current item in menu, or use current route?
-- When a route is selected, calculate next and previous route
-- Store a Maybe of next and previous route? Must also be calculated initially


getNextItemInMenu : Route -> MenuItemList -> Maybe Route
getNextItemInMenu currentItemId menuItemList =
    let
        currentItemContainer =
            menuItemList
                |> List.map (findCurrentMenuItemContainer currentItemId)
                |> List.head
                |> Maybe.withDefault Nothing

        nextItem =
            currentItemContainer
                |> Maybe.map (\container -> findCurrentMenuItemContainer currentItemId)
    in
    Just (ExampleRoute 1)



-- Steps
-- Find the menuContainer of the current item by a recursive search
-- In the parent menuItem, find next item in list


findCurrentMenuItemContainer : Route -> MenuItem -> Maybe MenuItem
findCurrentMenuItemContainer currentItemRoute (MenuNode sketchInfo items) =
    let
        currentItemRoutePath =
            pathFor currentItemRoute
    in
    case sketchInfo of
        SketchMenuContainer _ ->
            items
                |> List.map (findCurrentMenuItemContainer currentItemRoute)
                |> List.head
                |> Maybe.withDefault Nothing

        SketchMenuItem title menuRoute ->
            if pathFor menuRoute == currentItemRoutePath then
                Just (MenuNode sketchInfo items)

            else
                Nothing


findNextItemInContainer : Route -> MenuItem -> Maybe MenuItem
findNextItemInContainer currentItemRoute (MenuNode sketchInfo items) =
    let
        currentItemRoutePath =
            pathFor currentItemRoute
    in
    case sketchInfo of
        SketchMenuContainer _ ->
            items
                |> List.filter isSketchMenuItem
                -- |> List.map (findCurrentMenuItemContainer currentItemRoute)
                |> List.head

        -- |> Maybe.withDefault Nothing
        SketchMenuItem _ _ ->
            Nothing


menuItemMatchesRoute : Route -> MenuItem -> Bool
menuItemMatchesRoute route (MenuNode sketchInfo _) =
    case sketchInfo of
        SketchMenuContainer _ ->
            False

        SketchMenuItem _ itemRoute ->
            if pathFor itemRoute == pathFor route then
                True

            else
                False


isSketchMenuItem : MenuItem -> Bool
isSketchMenuItem (MenuNode sketchInfo _) =
    case sketchInfo of
        SketchMenuContainer _ ->
            False

        _ ->
            True


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
