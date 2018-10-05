module Main exposing (main)

import Browser exposing (Document, document)
import Html exposing (..)
import Json.Decode as Decode exposing (Value)


type alias Model =
    { count : Int
    }


type Msg
    = Count


init : Value -> ( Model, Cmd msg )
init _ =
    ( { count = 0 }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd msg )
update msg model =
    case msg of
        Count ->
            ( { count = model.count + 1 }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "WAHEY IT WORKS"
    , body =
        [ Html.h1 [] [ text "nice one!" ]
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
