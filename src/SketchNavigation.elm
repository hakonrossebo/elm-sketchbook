module SketchNavigation exposing (SketchItem(..), TreeToC, chapters, viewToC)

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
        ]



-- chapters =
--     Chapter "Sketches"
--         [ Chapter "Nested 1"
--             [ Item "Sketch item 1" SketchItem1
--             , Item "Sketch item 2" SketchItem2
--             ]
--         ]


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
