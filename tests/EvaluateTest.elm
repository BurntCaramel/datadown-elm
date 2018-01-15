module EvaluateTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Expect
import Datadown.Tokenize exposing (..)
import Datadown.Evaluate exposing (..)


resolveIdentifier : String -> Maybe Value
resolveIdentifier identifier =
    case identifier of
        "five" ->
            Just (Float 5)

        "six" ->
            Just (Float 6)

        _ ->
            Nothing


suite : Test
suite =
    describe "Run"
        [ describe "resolveFloat"
            [ test "2 + 5" <|
                \_ ->
                    resolveFloat 2 resolveIdentifier [ Operator Add, Value (Float 5) ]
                        |> Expect.equal
                            (Ok 7)
            , test "2 - 5" <|
                \_ ->
                    resolveFloat 2 resolveIdentifier [ Operator Subtract, Value (Float 5) ]
                        |> Expect.equal
                            (Ok -3)
            , test "2 + five" <|
                \_ ->
                    resolveFloat 2 resolveIdentifier [ Operator Add, Identifier "five" ]
                        |> Expect.equal
                            (Ok 7)
            , test "2 - five" <|
                \_ ->
                    resolveFloat 2 resolveIdentifier [ Operator Subtract, Identifier "five" ]
                        |> Expect.equal
                            (Ok -3)
            ]
        , describe "resolveTokens"
            [ test "2 + 5" <|
                \_ ->
                    resolveTokens resolveIdentifier [ Value (Float 2), Operator Add, Value (Float 5) ]
                        |> Expect.equal
                            (Ok [ Value (Float 7) ])
            , test "2 + five" <|
                \_ ->
                    resolveTokens resolveIdentifier [ Value (Float 2), Operator Add, Identifier "five" ]
                        |> Expect.equal
                            (Ok [ Value (Float 7) ])
            ]
        ]
