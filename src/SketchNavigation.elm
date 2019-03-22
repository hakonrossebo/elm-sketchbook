module SketchNavigation exposing (SketchItem(..), TreeToC, chapters, examplesMenu, menu, viewMenu, viewToC)

import Array
import Html exposing (Html, div, h1, h2, h3, img, li, text, ul)
import Html.Attributes exposing (class, src)


type SketchItem
    = SketchItem1
    | SketchItem2


type TreeToC
    = Item String SketchItem
    | Chapter String (List TreeToC)


chapters =
    Chapter "Sketches"
        [ Item "Sketch 1" SketchItem1
        , Item "Sketch 2" SketchItem2
        , Item "Sketch 3" SketchItem2
        ]



-- chapters =
--     Chapter "Sketches"
--         [ Chapter "Nested 1"
--             [ Item "Sketch item 1" SketchItem1
--             , Item "Sketch item 2" SketchItem2
--             ]
--         ]


type SketchMenuInfo
    = SketchMenuContainer String
    | SketchMenuItem String SketchItem


type MenuItem
    = MenuNode SketchMenuInfo (List MenuItem)


menu =
    MenuNode
        (SketchMenuContainer
            "Sketches M"
        )
        [ MenuNode (SketchMenuItem "Sketch 1" SketchItem1) []
        , MenuNode (SketchMenuItem "Sketch 2" SketchItem2) []
        , MenuNode (SketchMenuItem "Sketch 3" SketchItem2) []
        ]


examplesMenu =
    MenuNode
        (SketchMenuContainer
            "Examples"
        )
        [ MenuNode (SketchMenuItem "Example 1" SketchItem1) []
        , MenuNode (SketchMenuItem "Example 2" SketchItem2) []
        , MenuNode (SketchMenuItem "Example 3" SketchItem2) []
        ]


getMenuContainerItemLength : MenuItem -> Int
getMenuContainerItemLength (MenuNode _ items) =
    List.length items



-- starte med en posisjon som gir navn og item
-- skal kunne klikke på et meny item og sette item som current model
-- Bruke navigation med url?
-- Frem og tilbake knapp for å kunne navigere til neste?
-- Vise valgt item i meny


viewMenus : List MenuItem -> Html msg
viewMenus menuItems =
    div [] []


viewMenu : MenuItem -> Html msg
viewMenu (MenuNode info items) =
    case info of
        SketchMenuContainer title ->
            let
                subChapters =
                    List.map viewMenu items
            in
            ul []
                [ li []
                    [ h3 [] [ text title ]
                    ]
                , ul [] subChapters
                ]

        SketchMenuItem title _ ->
            let
                subChapters =
                    List.map viewMenu items
            in
            ul []
                [ li []
                    [ h3 [] [ text title ]
                    ]
                , ul [] subChapters
                ]


viewToC : TreeToC -> Html msg
viewToC tree =
    case tree of
        Item title item ->
            li [] [ text title ]

        Chapter title items ->
            let
                subChapters =
                    List.map viewToC items
            in
            ul []
                [ li []
                    [ h3 [] [ text title ]
                    ]
                , ul [] subChapters
                ]
