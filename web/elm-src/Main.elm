module Main exposing (..)

import Html exposing (..)


main : Program Never Model msg
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


update : msg -> Model -> ( Model, Cmd msg )
update msg model =
    model ! []



-- Subscriptions


subscriptions : Model -> Sub msg
subscriptions _ =
    Sub.none



-- View


view : Model -> Html msg
view model =
    div []
        [ text "Hello there!" ]
