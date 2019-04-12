module Shared exposing (CommonSketchInformation, SharedModel)


type alias CommonSketchInformation =
    { title : String
    , markdown : String
    }


type alias SharedModel a =
    { a
        | info : CommonSketchInformation
    }
