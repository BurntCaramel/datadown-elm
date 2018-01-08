module Datadown.Process
    exposing
        ( processDocument
        )

{-| Content


# Types

@docs processDocument

-}

import Dict exposing (Dict(..))
import Datadown exposing (Document, Section)
import Datadown.Content exposing (Content(..))


type Error
    = Invalid
    | UnknownKind


processHTMLSection : Section -> a -> Result Error Content
processHTMLSection section resolve =
    case section.mainContent of
        Just (Code language codeText) ->
            Ok (Code language codeText)

        _ ->
            Err Invalid


processSection : Section -> Dict String (Result Error Content) -> Dict String (Result Error Content)
processSection section prevResults =
    let
        result =
            case section.title of
                "html" ->
                    processHTMLSection section prevResults
                
                _ ->
                    Err UnknownKind
    in
        Dict.insert section.title result prevResults

{-| Process a document and return a result
-}
processDocument : Document -> Dict String (Result Error Content)
processDocument document =
    document.sections
        |> List.foldl processSection Dict.empty
    