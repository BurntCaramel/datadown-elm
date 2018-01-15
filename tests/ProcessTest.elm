module ProcessTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Expect
import Dict
import Datadown exposing (Content(..))
import Datadown.Process exposing (processDocument)


reverseExpressions : String -> Result String String
reverseExpressions input =
    case input of
        "error" ->
            Err "error"
        
        _ ->
            input
                |> String.reverse
                |> Ok


suite : Test
suite =
    describe "Processing"
        [ describe "Processing html documents"
            [ test "HTML with variable" <|
                \() ->
                    processDocument reverseExpressions
                        { title = "The title"
                        , sections =
                            [ { title = "message", mainContent = Just (Text "hello!"), secondaryContent = Dict.empty }
                            , { title = "html", mainContent = Just (Code (Just "html") "<div>{{ message }}</div>"), secondaryContent = Dict.empty }
                            ]
                        }
                    |> Expect.equal
                        (Dict.fromList [ ( "message", Ok (Text "hello!") ), ( "html", Ok (Code (Just "html") "<div>hello!</div>") ) ])
            , test "HTML with two levels of variables" <|
                \() ->
                    processDocument reverseExpressions
                        { title = "The title"
                        , sections =
                            [ { title = "firstName", mainContent = Just (Text "Jane"), secondaryContent = Dict.empty }
                            , { title = "message", mainContent = Just (Text "hello {{ firstName }}!"), secondaryContent = Dict.empty }
                            , { title = "html", mainContent = Just (Code (Just "html") "<div>{{ message }}</div>"), secondaryContent = Dict.empty }
                            ]
                        }
                    |> Expect.equal
                        (Dict.fromList [ ( "firstName", Ok (Text "Jane") ), ( "message", Ok (Text "hello Jane!") ), ( "html", Ok (Code (Just "html") "<div>hello Jane!</div>") ) ])
            ]
        , describe "Evaluating expressions"
            [ test "Number" <|
                \() ->
                    processDocument reverseExpressions
                        { title = "The title"
                        , sections =
                            [ { title = "reverse", mainContent = Just (Expressions "hello"), secondaryContent = Dict.empty }
                            ]
                        }
                    |> Expect.equal
                        (Dict.fromList [ ( "reverse", Ok (Expressions "olleh") ) ])
            ]
        ]
