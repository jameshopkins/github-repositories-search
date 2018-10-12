module Main exposing
    ( SortCriterion(..)
    , main
    , populateLanguageFilterValues
    , resultsPaginationParser
    , sortResults
    , updateFilters
    )

import Browser exposing (Document, document)
import Css exposing (..)
import Dict exposing (Dict)
import Html.Styled exposing (Html, a, button, div, form, h1, h2, h3, img, input, label, li, option, p, select, styled, text, toUnstyled, ul)
import Html.Styled.Attributes exposing (href, src, type_, value)
import Html.Styled.Events exposing (onCheck, onInput, onSubmit)
import Http
import Iso8601 as CrappyDateString
import Json.Decode as Decode exposing (Decoder, Value)
import LinkHeader
import List.Extra as List
import Set
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


resultsPaginationParser : String -> Maybe Int
resultsPaginationParser raw =
    case raw |> LinkHeader.parse |> List.last of
        Just { rel } ->
            case rel of
                LinkHeader.RelLast page ->
                    Just page

                _ ->
                    Nothing

        Nothing ->
            Nothing


resultsDecoder : Http.Response String -> Result String Projects
resultsDecoder { body, headers } =
    let
        bodyResponse =
            body
                |> Decode.decodeString (Decode.field "items" (Decode.list projectDecoder))
                |> Result.mapError Decode.errorToString

        paginationHeader =
            headers
                |> Dict.get "link"
    in
    Debug.log (Debug.toString paginationHeader)
        bodyResponse


submitQuery : String -> Cmd Msg
submitQuery query =
    let
        request =
            Http.request
                { method = "GET"
                , headers = []
                , body = Http.emptyBody
                , withCredentials = False
                , timeout = Nothing
                , url = "https://api.github.com/search/repositories?q=" ++ query
                , expect = Http.expectStringResponse resultsDecoder
                }
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
    styled Html.Styled.form
        [ flexBasis (pct 100) ]
        [ onSubmit SubmitQuery ]
        [ input [ type_ "text", onInput MutateQuery ] []
        , button [ type_ "submit", query |> String.isEmpty |> Html.Styled.Attributes.disabled ] [ text "Search!" ]
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
renderSearchResult { description, name, lastUpdated, owner, score, url } =
    let
        ownerInfo =
            div []
                [ styled img [ Css.width <| px 50 ] [ src owner.avatar ] [], text owner.name ]

        formattedDescription =
            case description of
                Just desc ->
                    p [] [ text desc ]

                Nothing ->
                    text ""
    in
    li []
        [ a [ href url, Html.Styled.Attributes.target "_blank" ] [ styled h3 [ fontSize <| px 30 ] [] [ text name ] ]
        , ownerInfo
        , formattedDescription
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
    styled div
        [ flex (int 4) ]
        []
        [ ul [] <|
            List.map renderSearchResult resultsList
        , loading
        ]


renderFilter : String -> Filter -> Html Msg
renderFilter attribute filter =
    let
        renderFilterItem ( name, selected ) =
            styled label
                [ displayFlex, justifyContent spaceBetween ]
                []
                [ text name
                , input [ onCheck (UpdateFilterValue attribute name), type_ "checkbox", Html.Styled.Attributes.checked selected ] []
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
    div []
        [ h2 [] [ text "Sort By" ]
        , select [ onInput UpdateSort ]
            [ option [ value "relevance" ] [ text "Relevance" ]
            , option [ value "last-updated" ] [ text "Last Updated" ]
            ]
        ]


view : Model -> Document Msg
view { filters, results, sort, query } =
    let
        resultsView =
            if not <| List.isEmpty results.all then
                styled div
                    [ flex (int 1) ]
                    []
                    [ renderSort
                    , renderFilters filters
                    ]

            else
                text ""

        body =
            styled div
                [ displayFlex, flexWrap wrap ]
                []
                [ styled h1 [ flexBasis (pct 100) ] [] [ text "Search Github Projects" ]
                , searchQuery query
                , resultsView
                , renderSearchResults results filters sort
                ]
    in
    { title = "Github Repository Search"
    , body = [ toUnstyled body ]
    }


main : Program Value Model Msg
main =
    document
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
