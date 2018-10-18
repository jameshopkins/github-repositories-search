module MainTest exposing (suite)

import Dict
import Expect
import Main exposing (SortCriterion(..), populateLanguageFilterValues, resultsPaginationParser, sortResults, updateFilters)
import Parser
import Test exposing (Test, describe, test)
import Time exposing (millisToPosix)


suite : Test
suite =
    describe "Main"
        [ describe "resultsPaginationParser"
            [ test "Wahey" <|
                let
                    expected =
                        Just 34

                    rawString =
                        "<https://api.github.com/search/repositories?q=bbc&page=2>; rel=\"next\", <https://api.github.com/search/repositories?q=bbc&page=34>; rel=\"last\""
                in
                \_ -> resultsPaginationParser rawString |> Expect.equal expected
            ]
        , describe "sortResults"
            [ test "Sorts results given a last updated criterion" <|
                let
                    projects =
                        [ { name = "Foo"
                          , owner = { name = "James", avatar = "Some avatar" }
                          , url = "Foo URL!!!"
                          , lastUpdated = millisToPosix 1538998604
                          , description = Just "Some description"
                          , language = "Elm"
                          , score = 1.92
                          }
                        , { name = "Alba"
                          , owner = { name = "Natalie", avatar = "Some avatar" }
                          , url = "Alba has a URL?!"
                          , lastUpdated = millisToPosix 1233998500
                          , description = Just "Alba is cute"
                          , language = "Elm"
                          , score = 5.77
                          }
                        , { name = "Ermmm jQuery is still cool...?"
                          , owner = { name = "Definitely not me", avatar = "Some avatar" }
                          , url = ":("
                          , lastUpdated = millisToPosix 1633998500
                          , description = Nothing
                          , language = "jQuery"
                          , score = 9.21
                          }
                        ]

                    expected =
                        [ { name = "Ermmm jQuery is still cool...?"
                          , owner = { name = "Definitely not me", avatar = "Some avatar" }
                          , url = ":("
                          , lastUpdated = millisToPosix 1633998500
                          , description = Nothing
                          , language = "jQuery"
                          , score = 9.21
                          }
                        , { name = "Foo"
                          , owner = { name = "James", avatar = "Some avatar" }
                          , url = "Foo URL!!!"
                          , lastUpdated = millisToPosix 1538998604
                          , description = Just "Some description"
                          , language = "Elm"
                          , score = 1.92
                          }
                        , { name = "Alba"
                          , owner = { name = "Natalie", avatar = "Some avatar" }
                          , url = "Alba has a URL?!"
                          , lastUpdated = millisToPosix 1233998500
                          , description = Just "Alba is cute"
                          , language = "Elm"
                          , score = 5.77
                          }
                        ]
                in
                \_ -> sortResults LastUpdated projects |> Expect.equal expected
            , test "Sorts results given a score criterion" <|
                let
                    projects =
                        [ { name = "Foo"
                          , owner = { name = "James", avatar = "Some avatar" }
                          , url = "Foo URL!!!"
                          , lastUpdated = millisToPosix 1538998604
                          , description = Just "Some description"
                          , language = "Elm"
                          , score = 1.92
                          }
                        , { name = "Alba"
                          , owner = { name = "Natalie", avatar = "Some avatar" }
                          , url = "Alba has a URL?!"
                          , lastUpdated = millisToPosix 1233998500
                          , description = Just "Alba is cute"
                          , language = "Elm"
                          , score = 5.77
                          }
                        , { name = "Ermmm jQuery is still cool...?"
                          , owner = { name = "Definitely not me", avatar = "Some avatar" }
                          , url = ":("
                          , lastUpdated = millisToPosix 1633998500
                          , description = Nothing
                          , language = "jQuery"
                          , score = 9.21
                          }
                        ]

                    expected =
                        [ { name = "Ermmm jQuery is still cool...?"
                          , owner = { name = "Definitely not me", avatar = "Some avatar" }
                          , url = ":("
                          , lastUpdated = millisToPosix 1633998500
                          , description = Nothing
                          , language = "jQuery"
                          , score = 9.21
                          }
                        , { name = "Alba"
                          , owner = { name = "Natalie", avatar = "Some avatar" }
                          , url = "Alba has a URL?!"
                          , lastUpdated = millisToPosix 1233998500
                          , description = Just "Alba is cute"
                          , language = "Elm"
                          , score = 5.77
                          }
                        , { name = "Foo"
                          , owner = { name = "James", avatar = "Some avatar" }
                          , url = "Foo URL!!!"
                          , lastUpdated = millisToPosix 1538998604
                          , description = Just "Some description"
                          , language = "Elm"
                          , score = 1.92
                          }
                        ]
                in
                \_ -> sortResults Score projects |> Expect.equal expected
            ]
        , describe "updateFilters"
            [ test "Updates a filter value" <|
                let
                    filterItems =
                        [ ( "C#", True )
                        , ( "Haskell", True )
                        , ( "Elm", False )
                        ]
                            |> Dict.fromList

                    filters =
                        Dict.singleton "language" filterItems

                    expectedFilterItems =
                        [ ( "C#", True )
                        , ( "Haskell", True )
                        , ( "Elm", True )
                        ]
                            |> Dict.fromList

                    expectedFilters =
                        Dict.singleton "language" expectedFilterItems
                in
                \_ -> updateFilters filters "language" "Elm" True |> Expect.equal expectedFilters
            ]
        , describe "populateLanguageFilterValues"
            [ test "Combines language filter values specific to the API response" <|
                let
                    filters =
                        Dict.singleton "language" Dict.empty

                    projects =
                        [ { name = "Foo"
                          , owner = { name = "James", avatar = "Some avatar" }
                          , url = "Foo URL!!!"
                          , lastUpdated = millisToPosix 1538998604
                          , description = Just "Some description"
                          , language = "Elm"
                          , score = 1.92
                          }
                        , { name = "Alba"
                          , owner = { name = "Natalie", avatar = "Some avatar" }
                          , url = "Alba has a URL?!"
                          , lastUpdated = millisToPosix 1538998500
                          , description = Just "Alba is cute"
                          , language = "Elm"
                          , score = 5.77
                          }
                        , { name = "Ermmm jQuery is still cool...?"
                          , owner = { name = "Definitely not me", avatar = "Some avatar" }
                          , url = ":("
                          , lastUpdated = millisToPosix 1538998550
                          , description = Nothing
                          , language = "jQuery"
                          , score = 9.21
                          }
                        , { name = "Elixir"
                          , owner = { name = "james", avatar = "Some avatar" }
                          , url = ":("
                          , lastUpdated = millisToPosix 1538998250
                          , description = Just "I love Elixir!!"
                          , language = "Elixir"
                          , score = 10.92
                          }
                        ]

                    expectedFilterItems =
                        [ ( "Elm", True ), ( "jQuery", True ), ( "Elixir", True ) ] |> Dict.fromList

                    expectedFilter =
                        Dict.singleton "language" expectedFilterItems
                in
                \_ -> populateLanguageFilterValues filters projects |> Expect.equal expectedFilter
            ]
        ]
