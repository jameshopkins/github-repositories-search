module Main exposing (SortCriterion(..), main, populateLanguageFilterValues, sortResults, updateFilters)

import Browser exposing (Document, document)
import Dict exposing (Dict)
import Html exposing (Html, a, button, div, form, h1, h2, h3, input, label, li, option, select, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onInput, onSubmit)
import Http
import Iso8601 as CrappyDateString
import Json.Decode as Decode exposing (Decoder, Value)
import List.Extra as List
import Time exposing (Posix)


type alias Model =
    { query : String
    , results : Results
    , filters : Filters
    , sort : SortCriterion
    }


type alias Filters =
    Dict String Filter


type alias Filter =
    Dict String Bool


type alias Owner =
    { name : String
    , avatar : String
    }


type alias Project =
    { name : String
    , owner : Owner
    , url : String
    , lastUpdated : Posix
    , description : Maybe String
    , language : String
    , score : Float
    }


type alias Projects =
    List Project


type alias Results =
    { all : Projects
    , transaction : ProjectsFetchTransaction
    }


type ProjectsFetchTransaction
    = NotAsked
    | Loading
    | Failure
    | Success Projects


type Msg
    = SubmitQuery
    | MutateQuery String
    | SearchResult (Result Http.Error Projects)
    | UpdateFilterValue String String Bool
    | UpdateSort String


type SortCriterion
    = LastUpdated
    | Score


init : Value -> ( Model, Cmd msg )
init _ =
    ( Model "" (Results [] NotAsked) Dict.empty Score, Cmd.none )


submitQuery : String -> Cmd Msg
submitQuery query =
    let
        decoder =
            Decode.field "items" (Decode.list projectDecoder)

        request =
            Http.get
                ("https://api.github.com/search/repositories?q="
                    ++ query
                )
                decoder
    in
    Http.send SearchResult request


ownerDecoder : Decoder Owner
ownerDecoder =
    Decode.map2 Owner
        (Decode.field "login" Decode.string)
        (Decode.field "avatar_url" Decode.string)


projectDecoder : Decoder Project
projectDecoder =
    let
        languageDecoder =
            Decode.nullable Decode.string
                |> Decode.map (Maybe.withDefault "NO LANGUAGE")
    in
    Decode.map7 Project
        (Decode.field "name" Decode.string)
        (Decode.field "owner" ownerDecoder)
        (Decode.field "html_url" Decode.string)
        (Decode.field "updated_at" CrappyDateString.decoder)
        (Decode.field "description" (Decode.nullable Decode.string))
        (Decode.field "language" languageDecoder)
        (Decode.field "score" Decode.float)


populateLanguageFilterValues : Filters -> Projects -> Filters
populateLanguageFilterValues filters projects =
    let
        languages =
            projects
                |> List.map (\{ language } -> ( language, True ))
                |> List.uniqueBy (\( language, _ ) -> language)
                |> Dict.fromList
    in
    filters
        |> Dict.insert "language" languages


updateFilters : Filters -> String -> String -> Bool -> Filters
updateFilters filters filterType filterItem selectedState =
    let
        updatedFilter =
            Dict.get filterType filters
                |> Maybe.withDefault Dict.empty
                |> Dict.insert filterItem selectedState
    in
    filters
        |> Dict.insert filterType updatedFilter


normaliseSortCriterion : String -> SortCriterion
normaliseSortCriterion criterion =
    case criterion of
        "last-updated" ->
            LastUpdated

        _ ->
            Score


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modelResults =
            model.results
    in
    case msg of
        MutateQuery val ->
            ( { model | query = val }, Cmd.none )

        SubmitQuery ->
            ( { model | results = { modelResults | transaction = Loading } }, submitQuery model.query )

        UpdateFilterValue filterType filterItem selectedState ->
            let
                updatedFilters =
                    updateFilters model.filters filterType filterItem selectedState
            in
            ( { model | filters = updatedFilters }, Cmd.none )

        UpdateSort criterion ->
            ( { model | sort = normaliseSortCriterion criterion }, Cmd.none )

        SearchResult (Ok projects) ->
            ( { model
                | filters = populateLanguageFilterValues model.filters projects
                , results =
                    { modelResults | transaction = NotAsked, all = projects }
              }
            , Cmd.none
            )

        SearchResult (Err err) ->
            ( { model | results = { modelResults | transaction = Failure } }, Cmd.none )


searchQuery : String -> Html Msg
searchQuery query =
    Html.form [ onSubmit SubmitQuery ]
        [ input [ type_ "text", onInput MutateQuery ] []
        , button [ type_ "submit", query |> String.isEmpty |> disabled ] [ text "Search!" ]
        ]


filterResults : List ( String, Bool ) -> Projects -> Projects
filterResults languageFilter projects =
    let
        predicate project =
            List.any (\( language, selected ) -> language == project.language && selected == True) languageFilter
    in
    List.filter predicate projects


sortResults : SortCriterion -> Projects -> Projects
sortResults criterion projects =
    let
        descending a b =
            case compare a b of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT

        comparator a b =
            case criterion of
                LastUpdated ->
                    descending
                        (Time.posixToMillis a.lastUpdated)
                        (Time.posixToMillis b.lastUpdated)

                Score ->
                    descending a.score b.score
    in
    List.sortWith comparator projects


renderSearchResult : Project -> Html Msg
renderSearchResult { name, lastUpdated, score, url } =
    li []
        [ a [ href url, target "_blank" ] [ score |> String.fromFloat |> text ]
        ]


renderSearchResults : Results -> Filters -> SortCriterion -> Html Msg
renderSearchResults { all, transaction } filters sort =
    let
        loading =
            case transaction of
                Loading ->
                    text "Loading"

                _ ->
                    text ""

        languageFilter =
            filters
                |> Dict.get "language"
                |> Maybe.withDefault Dict.empty
                |> Dict.toList

        resultsList =
            all
                |> filterResults languageFilter
                |> sortResults sort
    in
    div []
        [ ul [] <|
            List.map renderSearchResult resultsList
        , loading
        ]


renderFilter : String -> Filter -> Html Msg
renderFilter attribute filter =
    let
        renderFilterItem ( name, selected ) =
            label []
                [ text name
                , input [ onCheck (UpdateFilterValue attribute name), type_ "checkbox", checked selected ] []
                ]
    in
    div
        []
        [ h3 [] [ text attribute ]
        , div [] (filter |> Dict.toList |> List.map renderFilterItem)
        ]


renderFilters : Filters -> Html Msg
renderFilters filters =
    div []
        [ h2 [] [ text "Filters" ]
        , case Dict.get "language" filters of
            Just language ->
                renderFilter "language" language

            Nothing ->
                text ""
        ]


renderSort : Html Msg
renderSort =
    select [ onInput UpdateSort ]
        [ option [ value "relevance" ] [ text "Relevance" ]
        , option [ value "last-updated" ] [ text "Last Updated" ]
        ]


view : Model -> Document Msg
view { filters, results, sort, query } =
    let
        resultsView =
            if not <| List.isEmpty results.all then
                div []
                    [ renderSort
                    , renderFilters filters
                    ]

            else
                text ""
    in
    { title = "Github Repository Search"
    , body =
        [ h1 []
            [ text "Search Github Projects!" ]
        , searchQuery query
        , resultsView
        , renderSearchResults results filters sort
        ]
    }


main : Program Value Model Msg
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
