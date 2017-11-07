port module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- Model


type alias Model =
    { pdbFile : Maybe String
    }



-- Init


init : ( Model, Cmd msg )
init =
    (Model Nothing) ! []



-- Update


type Msg
    = GetInputPDB


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInputPDB ->
            model ! [ requestPDBFile () ]



-- Ports


port requestPDBFile : () -> Cmd msg



-- Subscriptions


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html Msg
view model =
    div []
        [ text "Hello there!"
        , input [ type_ "file", id "pdbFileToLoad" ] []
        , button [ onClick GetInputPDB ] [ text "Upload" ]
        ]
