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
    | ProcessPDBInput String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInputPDB ->
            model ! [ requestPDBFile () ]

        ProcessPDBInput pdbString ->
            { model | pdbFile = Just pdbString } ! []



-- Ports


port requestPDBFile : () -> Cmd msg


port receivePDBFile : (String -> msg) -> Sub msg



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    receivePDBFile ProcessPDBInput



-- View


view : Model -> Html Msg
view model =
    div []
        [ text "Hello there!"
        , input [ type_ "file", id "pdbFileToLoad" ] []
        , button [ onClick GetInputPDB ] [ text "Upload" ]
        ]
