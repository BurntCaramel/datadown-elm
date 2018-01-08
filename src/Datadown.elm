module Datadown
    exposing
        ( Document
        , Section
        )

{-| A library for Datadown parsing


# Types

@docs Document, Section

-}

import Dict exposing (Dict)
import Datadown.Content exposing (Content)


{-| A section of data
-}
type alias Section =
    { title : String
    , mainContent : Maybe Content
    , secondaryContent : Dict String Content
    }


{-| A full document, with many sections
-}
type alias Document =
    { title : String
    , properties : Dict String String
    , sections : List Section
    }
