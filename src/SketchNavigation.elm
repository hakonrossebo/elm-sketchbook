module SketchNavigation exposing (SketchItem(..), TreeToC, chapters, viewToC)

import Html exposing (Html, div, h1, img, li, text, ul)
import Html.Attributes exposing (class, src)


type SketchItem
    = SketchItem1
    | SketchItem2


type TreeToC
    = Item String SketchItem
    | Chapter String (List TreeToC)



-- chapters =
--     Chapter "Main"
--         [ Item "Sketch item 1" SketchItem1
--         , Item "Sketch item 2" SketchItem2
--         ]


chapters =
    Chapter "Main"
        [ Chapter "Nested 1"
            [ Item "Sketch item 1" SketchItem1
            , Item "Sketch item 2" SketchItem2
            ]
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
                [ li [] ([ text title ] ++ subChapters)
                ]
