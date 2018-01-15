module TokenizeTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Expect
import Datadown.Tokenize exposing (..)
import Parser exposing (run)


suite : Test
suite =
    describe "Tokenize"
        [ describe "token"
            [ test "identifier" <|
                \_ ->
                    Expect.equal
                        (run identifier "abc")
                        (Ok (Identifier "abc"))
            , test "add operator" <|
                \_ ->
                    Expect.equal
                        (run operator "+")
                        (Ok (Operator Add))
            , test "subtract operator" <|
                \_ ->
                    Expect.equal
                        (run operator "-")
                        (Ok (Operator Subtract))
            , test "token identifier" <|
                \_ ->
                    Expect.equal
                        (run token "abc")
                        (Ok (Identifier "abc"))
            , test "token float" <|
                \_ ->
                    run token "5"
                        |> Expect.equal (Ok (Value (Float 5)))
            , test "empty string" <|
                \_ ->
                    run operator ""
                        |> Expect.err
            ]
        , describe "tokens"
            [ test "identifier add identifier" <|
                \_ ->
                    Expect.equal
                        (run tokens "abc + def")
                        (Ok [ Identifier "abc", Operator Add, Identifier "def" ])
            , test "no whitespace" <|
                \_ ->
                    Expect.equal
                        (run tokens "abc+def")
                        (Ok [ Identifier "abc", Operator Add, Identifier "def" ])
            , test "much whitespace" <|
                \_ ->
                    Expect.equal
                        (run tokens "   abc    +    def      ")
                        (Ok [ Identifier "abc", Operator Add, Identifier "def" ])
            ]
        , describe "lines"
            [ test "lines with single line" <|
                \_ ->
                    Expect.equal
                        (run lines "abc + def")
                        (Ok [ [ Identifier "abc", Operator Add, Identifier "def" ] ])
            , test "lines with two lines" <|
                \_ ->
                    Expect.equal
                        (run lines "abc + def\nx - y")
                        (Ok
                            [ [ Identifier "abc", Operator Add, Identifier "def" ]
                            , [ Identifier "x", Operator Subtract, Identifier "y" ]
                            ]
                        )
            , test "lines with two lines much whitespace" <|
                \_ ->
                    Expect.equal
                        (run lines "\n\nabc + def\n\nx - y\n\n")
                        (Ok
                            [ [ Identifier "abc", Operator Add, Identifier "def" ]
                            , [ Identifier "x", Operator Subtract, Identifier "y" ]
                            ]
                        )
            ]
        ]
