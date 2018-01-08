module ParseTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Expect
import Dict
import Datadown.Parse exposing (parseDocument, parseSection)


suite : Test
suite =
    describe "Parsing"
        [ describe "Parsing documents"
            [ test "Title" <|
                \() ->
                    Expect.equal (parseDocument """
# The title
""") ({ title = "The title", properties = Dict.empty, sections = [] })
            , test "No title" <|
                \() ->
                    Expect.equal (parseDocument """
""") ({ title = "", properties = Dict.empty, sections = [] })
            ]
        , describe "Sections"
            [ test "Single section" <|
                \() ->
                    Expect.equal (parseDocument """
# The title

## First
""") (
    { title = "The title"
    , properties = Dict.empty
    , sections =
    [ { title = "First"
      , rawContent = ""
      }
    ]
    }
    )
            ]
        ]
