module Main exposing (main, populateLanguageFilterValues, updateFilters)

import Browser exposing (Document, document)
import Dict exposing (Dict)
import Html exposing (Html, button, div, form, h1, h2, h3, input, label, li, text, ul)
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


init : Value -> ( Model, Cmd msg )
init _ =
    ( Model "" (Results [] NotAsked) Dict.empty, Cmd.none )


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
    Decode.map6 Project
        (Decode.field "name" Decode.string)
        (Decode.field "owner" ownerDecoder)
        (Decode.field "url" Decode.string)
        (Decode.field "updated_at" CrappyDateString.decoder)
        (Decode.field "description" (Decode.nullable Decode.string))
        (Decode.field "language" languageDecoder)


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

        SearchResult res ->
            let
                projectsResult =
                    Result.withDefault [] res

                filters =
                    case res of
                        Ok projects ->
                            populateLanguageFilterValues model.filters projects

                        Err _ ->
                            model.filters
            in
            ( { model
                | filters = filters
                , results =
                    { modelResults | transaction = NotAsked, all = projectsResult }
              }
            , Cmd.none
            )


searchQuery : String -> Html Msg
searchQuery query =
    Html.form [ onSubmit SubmitQuery ]
        [ input [ type_ "text", onInput MutateQuery ] []
        , button [ type_ "submit", query |> String.isEmpty |> disabled ] [ text "Search!" ]
        ]


filterResults : Projects -> List ( String, Bool ) -> Projects
filterResults projects languageFilter =
    let
        predicate project =
            List.any (\( language, selected ) -> language == project.language && selected == True) languageFilter
    in
    List.filter predicate projects


searchResults : Results -> Filters -> Html Msg
searchResults { all, transaction } filters =
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
            filterResults all languageFilter
    in
    div []
        [ ul [] <| List.map (\project -> li [] [ text project.name ]) resultsList
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


view : Model -> Document Msg
view { filters, results, query } =
    { title = "Github Repository Search"
    , body =
        [ h1 []
            [ text "Search Github Projects!" ]
        , searchQuery query
        , renderFilters filters
        , searchResults results filters
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
