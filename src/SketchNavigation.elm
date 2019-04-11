module SketchNavigation exposing (Route(..), allMenus, examplesMenu, getNextItemInMenu, getPreviousItemInMenu, menu, parseUrl, pathFor, viewMenu, viewMenus)

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
        [ MenuNode (SketchMenuItem "Getting started" GettingStartedRoute) []
        , MenuNode (SketchMenuItem "Sketch 1" (SketchRoute 1)) []
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


getNextItemInMenu : Route -> MenuItemList -> Maybe Route
getNextItemInMenu currentItemId menuItemList =
    let
        currentItemContainer =
            menuItemList
                |> List.map (findCurrentMenuItemContainer currentItemId)
                |> List.filterMap identity
                |> List.head

        nextItem =
            currentItemContainer
                |> Maybe.andThen (\container -> findNextItemInContainer currentItemId container)
                |> Maybe.andThen routeForMenuItem
    in
    nextItem


getPreviousItemInMenu : Route -> MenuItemList -> Maybe Route
getPreviousItemInMenu currentItemId menuItemList =
    let
        currentItemContainer =
            menuItemList
                |> List.reverse
                |> List.map (findCurrentMenuItemContainer currentItemId)
                |> List.filterMap identity
                |> List.head

        nextItem =
            currentItemContainer
                |> Maybe.andThen (\container -> findPreviousItemInContainer currentItemId container)
                |> Maybe.andThen routeForMenuItem
    in
    nextItem


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
                |> List.filterMap identity
                |> List.head
                |> Maybe.andThen (\_ -> Just (MenuNode sketchInfo items))

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

        nextItem list =
            case list of
                [] ->
                    Nothing

                [ x ] ->
                    Nothing

                x :: xnext :: xs ->
                    if menuItemMatchesRoute currentItemRoute x then
                        Just xnext

                    else
                        nextItem (xnext :: xs)
    in
    case sketchInfo of
        SketchMenuContainer _ ->
            items
                |> List.filter isSketchMenuItem
                |> nextItem

        SketchMenuItem _ _ ->
            Nothing


findPreviousItemInContainer : Route -> MenuItem -> Maybe MenuItem
findPreviousItemInContainer currentItemRoute (MenuNode sketchInfo items) =
    let
        currentItemRoutePath =
            pathFor currentItemRoute

        nextItem list =
            case list of
                [] ->
                    Nothing

                [ x ] ->
                    Nothing

                x :: xnext :: xs ->
                    if menuItemMatchesRoute currentItemRoute x then
                        Just xnext

                    else
                        nextItem (xnext :: xs)
    in
    case sketchInfo of
        SketchMenuContainer _ ->
            items
                |> List.reverse
                |> List.filter isSketchMenuItem
                |> nextItem

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


routeForMenuItem : MenuItem -> Maybe Route
routeForMenuItem (MenuNode sketchInfo _) =
    case sketchInfo of
        SketchMenuItem _ route ->
            Just route

        _ ->
            Nothing


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
