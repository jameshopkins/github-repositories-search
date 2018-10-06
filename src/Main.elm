module Main exposing (main)

import Browser exposing (Document, document)
import Html exposing (Html, button, div, form, h1, input, li, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Iso8601 as CrappyDateString
import Json.Decode as Decode exposing (Decoder, Value)
import Time exposing (Posix)


type alias Model =
    { query : String
    , projects : Broadcast
    }


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
    }


type alias Projects =
    List Project


type Broadcast
    = NotAsked
    | Loading
    | Failure
    | Success Projects


type Msg
    = SubmitQuery
    | MutateQuery String
    | SearchResult (Result Http.Error Projects)


init : Value -> ( Model, Cmd msg )
init _ =
    ( Model "" NotAsked, Cmd.none )


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
    Decode.map5 Project
        (Decode.field "name" Decode.string)
        (Decode.field "owner" ownerDecoder)
        (Decode.field "url" Decode.string)
        (Decode.field "updated_at" CrappyDateString.decoder)
        (Decode.field "description" (Decode.nullable Decode.string))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MutateQuery val ->
            ( { model | query = val }, Cmd.none )

        SubmitQuery ->
            ( { model | projects = Loading }, submitQuery model.query )

        SearchResult res ->
            let
                projectsResult =
                    case res of
                        Ok projects ->
                            Success projects

                        Err _ ->
                            Failure
            in
            ( { model | projects = projectsResult }, Cmd.none )


searchQuery : Html Msg
searchQuery =
    Html.form [ onSubmit SubmitQuery ]
        [ input [ type_ "text", onInput MutateQuery ] []
        , button [ type_ "submit" ] [ text "Search!" ]
        ]


searchResults : Broadcast -> Html Msg
searchResults results =
    case results of
        NotAsked ->
            div [] [ text "Try searching!" ]

        Loading ->
            div [] [ text "Loading results" ]

        Failure ->
            div [] [ text "Failed!" ]

        Success projects ->
            ul [] <| List.map (\project -> li [] [ text project.name ]) projects


view : Model -> Document Msg
view model =
    { title = "Github Repository Search"
    , body =
        [ h1 []
            [ text "Search Github Projects!" ]
        , searchQuery
        , searchResults model.projects
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
