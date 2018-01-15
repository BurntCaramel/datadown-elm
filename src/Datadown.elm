module Datadown
    exposing
        ( Document
        , Section
        , Content(..)
        )

{-| A library for Datadown parsing


# Types

@docs Document, Section, Content

-}

import Dict exposing (Dict)


{-| Content, such as plain text, code, lists, etc
-}
type Content
    = Text String
    | Code (Maybe String) String -- ```html
    | Expressions String -- ```
    | List (List Content) -- -
    | Quote Document -- >


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
    , sections : List Section
    }
