module ProcessTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Expect
import Dict
import Datadown.Content exposing (Content(..))
import Datadown.Process exposing (processDocument)


suite : Test
suite =
    describe "Processing"
        [ describe "Processing html documents"
            [ test "HTML" <|
                \() ->
                    Expect.equal
                        (processDocument
                            { title = "The title"
                            , sections =
                                [ { title = "message", mainContent = Just (Text "hello!"), secondaryContent = Dict.empty }
                                , { title = "html", mainContent = Just (Code (Just "html") "<div>{{ message }}</div>"), secondaryContent = Dict.empty }
                                ]
                            }
                        )
                        (Dict.fromList [ ("message", Ok (Text "hello!")), ( "html", Ok (Code (Just "html") "<div>hello!</div>") ) ])
            ]
        ]
