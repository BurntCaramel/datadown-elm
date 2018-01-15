module ParseTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Expect
import Dict
import Datadown exposing (Content(..))
import Datadown.Parse exposing (parseDocument, parseSection)


suite : Test
suite =
    describe "Parsing"
        [ describe "Parsing documents"
            [ test "Title" <|
                \_ ->
                    parseDocument identity """
# The title
"""
                        |> Expect.equal { title = "The title", sections = [] }
            , test "No title" <|
                \_ ->
                    parseDocument identity """
"""
                        |> Expect.equal { title = "", sections = [] }
            ]
        , describe "Sections"
            [ test "Single section with title" <|
                \_ ->
                    parseDocument identity """
# The title

## First
"""
                        |> Expect.equal
                            { title = "The title"
                            , sections =
                                [ { title = "First"
                                  , mainContent = Nothing
                                  , secondaryContent = Dict.empty
                                  }
                                ]
                            }
            , test "Multiple sections with titles" <|
                \_ ->
                    parseDocument identity """
# The title

## First
## Second
## Third
"""
                        |> Expect.equal
                            { title = "The title"
                            , sections =
                                [ { title = "First"
                                  , mainContent = Nothing
                                  , secondaryContent = Dict.empty
                                  }
                                , { title = "Second"
                                  , mainContent = Nothing
                                  , secondaryContent = Dict.empty
                                  }
                                , { title = "Third"
                                  , mainContent = Nothing
                                  , secondaryContent = Dict.empty
                                  }
                                ]
                            }
            , test "Section with list content" <|
                \_ ->
                    parseDocument identity """
# The title

## First

- 1st
- 2nd
- 3rd
"""
                        |> Expect.equal
                            { title = "The title"
                            , sections =
                                [ { title = "First"
                                  , mainContent =
                                        Just
                                            (List
                                                [ Text "1st"
                                                , Text "2nd"
                                                , Text "3rd"
                                                ]
                                            )
                                  , secondaryContent = Dict.empty
                                  }
                                ]
                            }
            , test "Section with text content" <|
                \_ ->
                    parseDocument identity """
# The title

## First

Blah blah blah
"""
                        |> Expect.equal
                            { title = "The title"
                            , sections =
                                [ { title = "First"
                                  , mainContent =
                                        Just (Text "Blah blah blah")
                                  , secondaryContent = Dict.empty
                                  }
                                ]
                            }
            , test "Section with code content" <|
                \_ ->
                    parseDocument identity """
# The title

## First

```js
export default 42;
```
"""
                        |> Expect.equal
                            { title = "The title"
                            , sections =
                                [ { title = "First"
                                  , mainContent =
                                        Just
                                            (Code (Just "js") """export default 42;
""")
                                  , secondaryContent = Dict.empty
                                  }
                                ]
                            }
            , test "Section with expressions" <|
                \_ ->
                    parseDocument identity """
# The title

## First

```
5 + 7
```
"""
                        |> Expect.equal
                            { title = "The title"
                            , sections =
                                [ { title = "First"
                                  , mainContent =
                                        Just
                                            (Expressions """5 + 7
""")
                                  , secondaryContent = Dict.empty
                                  }
                                ]
                            }
            , test "Section with block quote" <|
                \_ ->
                    parseDocument identity """
# The title

## First

> ## Inner section
> Blah blah blah
"""
                        |> Expect.equal
                            { title = "The title"
                            , sections =
                                [ { title = "First"
                                  , mainContent =
                                        Just
                                            (Quote
                                                ({ title = ""
                                                 , sections =
                                                    [ { title = "Inner section"
                                                      , mainContent =
                                                            Just (Text "Blah blah blah")
                                                      , secondaryContent = Dict.empty
                                                      }
                                                    ]
                                                 }
                                                )
                                            )
                                  , secondaryContent = Dict.empty
                                  }
                                ]
                            }
            ]
        ]
