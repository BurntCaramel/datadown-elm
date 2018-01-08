module Datadown.Process
    exposing
        ( processDocument
        )

{-| Content


# Types

@docs processDocument

-}

import Dict exposing (Dict(..))
import Regex exposing (Regex)
import Datadown exposing (Document, Section)
import Datadown.Content exposing (Content(..))


type Error
    = Invalid
    | UnknownKind
    | NoContent


mustacheVariableRegex : Regex
mustacheVariableRegex = Regex.regex "{{\\s?(.+?)\\s?}}"

mustache : (String -> Maybe String) -> String -> String
mustache resolveVariable input =
    let
        replacer : Regex.Match -> String
        replacer match =
            let
                key = match.submatches
                    |> List.head
                    |> Maybe.withDefault Nothing
                    |> Maybe.withDefault ""
            in
                resolveVariable key
                    |> Maybe.withDefault ""
    in
        Regex.replace Regex.All mustacheVariableRegex replacer input


stringResolverForResults : Dict String (Result Error Content) -> (String -> Maybe String)
stringResolverForResults results =
    \key ->
        case Dict.get key results of
            Just (Ok result) ->
                case result of
                    Text text ->
                        Just text
                    
                    _ ->
                        Nothing
            
            _ ->
                Nothing

processHTMLSection : (String -> Maybe String) -> Section -> Result Error Content
processHTMLSection resolve section =
    case section.mainContent of
        Just (Code language codeText) ->
            Ok (Code language (mustache resolve codeText))

        Just content ->
            Ok content
        
        Nothing ->
            Err Invalid


processSection : Section -> Dict String (Result Error Content) -> Dict String (Result Error Content)
processSection section prevResults =
    let
        resolve : String -> Maybe String
        resolve =
            stringResolverForResults prevResults

        result : Result Error Content
        result =
            case section.title of
                "html" ->
                    processHTMLSection resolve section

                _ ->
                    case section.mainContent of
                        Just content ->
                            Ok content

                        Nothing ->
                            Err NoContent
                    -- Result.fromMaybe (Err NoContent) section.mainContent
    in
        Dict.insert section.title result prevResults


{-| Process a document and return a result
-}
processDocument : Document -> Dict String (Result Error Content)
processDocument document =
    document.sections
        |> List.foldl processSection Dict.empty
