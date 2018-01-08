module ParseTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Expect
import Dict
import Datadown.Content exposing (Content(..))
import Datadown.Parse exposing (parseDocument, parseSection)


suite : Test
suite =
    describe "Parsing"
        [ describe "Parsing documents"
            [ test "Title" <|
                \() ->
                    Expect.equal (parseDocument """
# The title
""") ({ title = "The title", sections = [] })
            , test "No title" <|
                \() ->
                    Expect.equal (parseDocument """
""") ({ title = "", sections = [] })
            ]
        , describe "Sections"
            [ test "Single section with title" <|
                \() ->
                    Expect.equal (parseDocument """
# The title

## First
""")
                        ({ title = "The title"
                         , sections =
                            [ { title = "First"
                              , mainContent = Nothing
                              , secondaryContent = Dict.empty
                              }
                            ]
                         }
                        )
            , test "Multiple sections with titles" <|
                \() ->
                    Expect.equal (parseDocument """
# The title

## First
## Second
## Third
""")
                        ({ title = "The title"
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
                        )
            , test "Section with list content" <|
                \() ->
                    Expect.equal (parseDocument """
# The title

## First

- 1st
- 2nd
- 3rd
""")
                        ({ title = "The title"
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
                        )
            , test "Section with code content" <|
                \() ->
                    Expect.equal (parseDocument """
# The title

## First

```js
export default 42;
```
""")
                        ({ title = "The title"
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
                        )
            ]
        ]
