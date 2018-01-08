module Datadown.Parse exposing
  ( parseDocument
  , parseSection
  )

{-| Parse Datadown documents

# Run Parsers
@docs parseDocument, parseSection

-}


import Datadown exposing (Document, Section)
-- import String
-- import Regex exposing (Regex)
import Dict
import Markdown.Block as Block exposing (Block(..))
import Markdown.Inline as Inline exposing (Inline(..))


-- tagsRegex : Regex
-- tagsRegex = Regex.regex "\\B#[A-Za-z0-9_-]+(:\\s*[^#]*)?"
-- tagKeyValueRegex : Regex
-- tagKeyValueRegex = Regex.regex "\\B#([a-zA-Z0-9-_]+)(:\\s*([^#]*))?"

-- mentionsRegex : Regex
-- mentionsRegex = Regex.regex "@([a-zA-Z0-9_-]+(?:\\.[a-zA-Z0-9-_]+)*)"

-- introductionsRegex : Regex
-- introductionsRegex = Regex.regex "^@([a-zA-Z0-9_-]+):[\\s]*(.*)"

-- reduceMultipleSpaces : String -> String
-- reduceMultipleSpaces = Regex.replace Regex.All (Regex.regex "\\s+") (\_ -> " ")

-- nonEmptyRegex : Regex
-- nonEmptyRegex = Regex.regex "\\S"

-- rejectEmptyStrings : List String -> List String
-- rejectEmptyStrings = List.filter (Regex.contains nonEmptyRegex)

-- parseIntroduction : String -> { introduction: Maybe String, rest: String }
-- parseIntroduction input =
--   input
--   |> Regex.find Regex.All introductionsRegex
--   |> \matches ->
--     case matches of
--       match::_ ->
--         case match.submatches of
--           introduction::rest::[] ->
--             case rest of
--               Just rest ->
--                 { introduction = introduction, rest = rest }
              
--               Nothing ->
--                 { introduction = introduction, rest = "" }
--           _ ->
--             { introduction = Nothing, rest = input }
    
--       _ ->
--         { introduction = Nothing, rest = input }

{-| Parses a Datadown section

    parseSection """
## Section

- first
- second
- third
"""
-}
parseSection : List (Block b i) -> Section
parseSection blocks =
  { title = ""
  , rawContent = ""
  }

processDocumentBlock : Block b i -> Document -> Document
processDocumentBlock block document =
  case block of
    Heading text 1 inlines ->
      { document | title = Inline.extractText inlines }
    
    Heading text 2 inlines ->
      { document
      | sections = { title = (Inline.extractText inlines), rawContent = "" } :: document.sections
      }

    _ ->
      document

{-| Parses a Datadown document

    parseDocument """
# Title
"""
-}
parseDocument : String -> Document
parseDocument input =
  let
    initialDocument =
      { title = ""
      , properties = Dict.empty
      , sections = []
      }
  in
    input
    |> Block.parse Nothing -- using Config.defaultOptions
    |> List.foldr processDocumentBlock initialDocument
