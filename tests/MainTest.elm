module MainTest exposing (suite)

import Dict
import Expect
import Main exposing (populateLanguageFilterValues, updateFilters)
import Test exposing (Test, describe, test)
import Time exposing (millisToPosix)


suite : Test
suite =
    describe "Main"
        [ describe "updateFilters"
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
                          }
                        , { name = "Alba"
                          , owner = { name = "Natalie", avatar = "Some avatar" }
                          , url = "Alba has a URL?!"
                          , lastUpdated = millisToPosix 1538998500
                          , description = Just "Alba is cute"
                          , language = "Elm"
                          }
                        , { name = "Ermmm jQuery is still cool...?"
                          , owner = { name = "Definitely not me", avatar = "Some avatar" }
                          , url = ":("
                          , lastUpdated = millisToPosix 1538998550
                          , description = Nothing
                          , language = "jQuery"
                          }
                        , { name = "Elixir"
                          , owner = { name = "james", avatar = "Some avatar" }
                          , url = ":("
                          , lastUpdated = millisToPosix 1538998250
                          , description = Just "I love Elixir!!"
                          , language = "Elixir"
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
