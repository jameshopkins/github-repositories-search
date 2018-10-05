module Main exposing (main)

import Browser exposing (Document, document)
import Html exposing (Html, button, form, h1, input, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onSubmit)
import Http
import Iso8601 as CrappyDateString
import Json.Decode as Decode exposing (Decoder, Value)
import Time exposing (Posix)


type alias Model =
    { query : String
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


type Msg
    = SubmitQuery
    | MutateQuery String
    | SearchResult (Result Http.Error (List Project))


init : Value -> ( Model, Cmd msg )
init _ =
    ( Model "", Cmd.none )


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
            ( { query = val }, Cmd.none )

        SubmitQuery ->
            ( model, submitQuery model.query )

        SearchResult res ->
            Debug.log (Debug.toString res)
                ( model, Cmd.none )


search : Html Msg
search =
    Html.form [ onSubmit SubmitQuery ]
        [ input [ type_ "text", onInput MutateQuery ] []
        , button [ type_ "submit" ] [ text "Search!" ]
        ]


view : Model -> Document Msg
view model =
    { title = "Github Repository Search"
    , body =
        [ h1 []
            [ text "Search Github Projects!" ]
        , search
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
