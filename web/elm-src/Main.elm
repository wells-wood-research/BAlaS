port module Main exposing (..)

{-| This module provides the main entry point to the BALS web application
front end.
-}

import Dict
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
    , chainVisibility : Maybe (Dict.Dict String Bool)
    }



-- Init


init : ( Model, Cmd msg )
init =
    (Model Nothing Nothing) ! [ initialiseViewer () ]



-- Update


type Msg
    = GetInputPDB
    | ProcessPDBInput String
    | ProcessStructureInfo (List String)
    | SetChainVisibility String Bool


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInputPDB ->
            model ! [ requestPDBFile () ]

        ProcessPDBInput pdbString ->
            { model | pdbFile = Just pdbString }
                ! [ showStructure pdbString ]

        ProcessStructureInfo chainVisibility ->
            { model
                | chainVisibility =
                    List.map (\l -> ( l, True )) chainVisibility
                        |> Dict.fromList
                        |> Just
            }
                ! []

        SetChainVisibility label visible ->
            case model.chainVisibility of
                Just chainVisibility ->
                    { model
                        | chainVisibility =
                            Dict.insert label visible chainVisibility
                                |> Just
                    }
                        ! [ setChainVisibility ( label, visible ) ]

                Nothing ->
                    model ! []



-- Ports


port initialiseViewer : () -> Cmd msg


port showStructure : String -> Cmd msg


port requestPDBFile : () -> Cmd msg


port receivePDBFile : (String -> msg) -> Sub msg


port receiveStructureInfo : (List String -> msg) -> Sub msg


port setChainVisibility : ( String, Bool ) -> Cmd msg



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receivePDBFile ProcessPDBInput
        , receiveStructureInfo ProcessStructureInfo
        ]



-- View


view : Model -> Html Msg
view model =
    div [ class "main-grid" ]
        [ div
            [ id "viewer" ]
            []
        , div
            [ class "control-panel" ]
            [ div []
                [ input [ type_ "file", id "pdbFileToLoad" ] []
                , button [ onClick GetInputPDB ] [ text "Upload" ]
                ]
            , div []
                ([ h3 [] [ text "Show/Hide Chains" ] ]
                    ++ case model.chainVisibility of
                        Just chainVisibility ->
                            [ div []
                                (Dict.toList chainVisibility
                                    |> List.map chainSelect
                                )
                            ]

                        Nothing ->
                            []
                )
            ]
        ]


chainSelect : ( String, Bool ) -> Html Msg
chainSelect ( label, visible ) =
    div []
        [ text ("Chain " ++ label)
        , input
            [ onCheck (SetChainVisibility label)
            , type_ "checkbox"
            , checked visible
            ]
            []
        ]
