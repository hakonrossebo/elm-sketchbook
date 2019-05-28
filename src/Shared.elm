module Shared exposing (CommonSketchInformation, SharedModel, calculateAverageFps)


type alias CommonSketchInformation =
    { title : String
    , markdown : String
    }


type alias SharedModel a =
    { a
        | info : CommonSketchInformation
    }


calculateAverageFps : Int -> Float -> Int
calculateAverageFps old new =
    let
        smoothing =
            0.95

        floatOld =
            toFloat old

        avg =
            (floatOld * smoothing) + (new * (1.0 - smoothing))
    in
    round avg
