module Datadown.Content
    exposing
        ( Content(..)
        )

{-| Content


# Types

@docs Content

-}


{-| Content, such as plain text, code, lists, etc
-}
type Content
    = Text String
    | Code (Maybe String) String
    | List (List Content)
