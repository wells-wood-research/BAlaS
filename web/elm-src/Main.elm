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
    , chainVisibility : Maybe (Dict.Dict ChainID Bool)
    , ligand : Maybe ChainID
    }


type alias ChainID =
    String



-- Init


init : ( Model, Cmd msg )
init =
    (Model Nothing Nothing Nothing) ! [ initialiseViewer () ]



-- Update


type Msg
    = GetInputPDB
    | ProcessPDBInput String
    | ProcessStructureInfo (List ChainID)
    | SetChainVisibility ChainID Bool
    | SetLigand ChainID
    | ClearLigand


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetInputPDB ->
            model ! [ requestPDBFile () ]

        ProcessPDBInput pdbString ->
            { model
                | pdbFile = Just pdbString
                , chainVisibility = Nothing
                , ligand = Nothing
            }
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

        SetLigand chainID ->
            case model.chainVisibility of
                Just chainVis ->
                    { model | ligand = Just chainID }
                        ! [ colourByLigand ( (Dict.keys chainVis), chainID ) ]

                Nothing ->
                    model ! []

        ClearLigand ->
            case model.pdbFile of
                Just pdb ->
                    { model | ligand = Nothing } ! [ showStructure pdb ]

                Nothing ->
                    { model | ligand = Nothing } ! []



-- Cmds
-- Ports


port initialiseViewer : () -> Cmd msg


port showStructure : String -> Cmd msg


port requestPDBFile : () -> Cmd msg


port receivePDBFile : (String -> msg) -> Sub msg


port receiveStructureInfo : (List ChainID -> msg) -> Sub msg


port setChainVisibility : ( ChainID, Bool ) -> Cmd msg


port colourByLigand : ( List ChainID, ChainID ) -> Cmd msg



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
                (case model.chainVisibility of
                    Just chainVisibility ->
                        [ h3 [] [ text "" ]
                        , table []
                            ([ tr []
                                [ th [] [ text "Chain" ]
                                , th [] [ text "Visibility" ]
                                , th [] [ text "Ligand" ]
                                ]
                             ]
                                ++ (Dict.toList chainVisibility
                                        |> List.map (chainSelect model.ligand)
                                   )
                            )
                        ]

                    Nothing ->
                        []
                )
            ]
        ]


chainSelect : Maybe ChainID -> ( ChainID, Bool ) -> Html Msg
chainSelect mLigandLabel ( label, visible ) =
    tr []
        [ td [] [ text (label) ]
        , td []
            [ input
                [ onCheck (SetChainVisibility label)
                , type_ "checkbox"
                , checked visible
                ]
                []
            ]
        , td []
            (case mLigandLabel of
                Just ligandLabel ->
                    if ligandLabel == label then
                        [ button [ onClick ClearLigand ]
                            [ text "Clear" ]
                        ]
                    else
                        []

                Nothing ->
                    [ button [ onClick <| SetLigand label ]
                        [ text "Set" ]
                    ]
            )
        ]
