port module Main exposing (..)

{-| This module provides the main entry point to the BALS web application
front end.
-}

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
    (Model Nothing) ! [ initialiseViewer () ]



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
            { model | pdbFile = Just pdbString }
                ! [ showStructure pdbString ]



-- Ports


port initialiseViewer : () -> Cmd msg


port showStructure : String -> Cmd msg


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
        [ div [ id "viewer", viewerStyle ]
            [ input [ type_ "file", id "pdbFileToLoad" ] []
            , button [ onClick GetInputPDB ] [ text "Upload" ]
            ]
        ]


viewerStyle : Attribute msg
viewerStyle =
    style
        [ ( "position", "fixed" )
        , ( "bottom", "0px" )
        , ( "top", "0px" )
        , ( "left", "0px" )
        , ( "right", "0px" )
        ]
