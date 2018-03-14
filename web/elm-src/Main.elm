port module Main exposing (..)

{-| This module provides the main entry point to the BALS web application
front end.
-}

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as JDe
import Json.Decode.Extra exposing ((|:))
import Json.Encode as JEn


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
    { appMode : AppMode
    , alanineScanModel : AlanineScanModel
    }


type AppMode
    = ScanSubmission


type alias JobID =
    String


emptyModel : Model
emptyModel =
    Model
        ScanSubmission
        emptyAlaScanModel


type alias ChainID =
    String


type alias AlanineScanModel =
    { alanineScanSub : AlanineScanSub
    , alanineScanJobs : List JobID
    }


emptyAlaScanModel : AlanineScanModel
emptyAlaScanModel =
    AlanineScanModel emptyScanSub []


type alias AlanineScanSub =
    { pdbFile : Maybe String
    , chainVisibility : Maybe (Dict.Dict ChainID Bool)
    , ligand : Maybe ChainID
    }


validScanSub : AlanineScanSub -> Bool
validScanSub { pdbFile, ligand } =
    let
        isJust mA =
            case mA of
                Just _ ->
                    True

                Nothing ->
                    False
    in
        isJust pdbFile && isJust ligand


emptyScanSub : AlanineScanSub
emptyScanSub =
    AlanineScanSub
        Nothing
        Nothing
        Nothing


encodeAlanineScanSub : AlanineScanSub -> JEn.Value
encodeAlanineScanSub scanSub =
    JEn.object
        [ ( "pdbFile", Maybe.withDefault "Invalid" scanSub.pdbFile |> JEn.string )
        , ( "ligand", Maybe.withDefault "Invalid" scanSub.ligand |> JEn.string )
        ]


type alias ResidueInfo =
    { chainID : ChainID
    , aminoAcid : String
    , residueNumber : Int
    }



-- Init


init : ( Model, Cmd msg )
init =
    emptyModel ! [ initialiseViewer () ]



-- Update


type Msg
    = SetAppMode AppMode
    | UpdateScanSubmission ScanMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAppMode appMode ->
            { model | appMode = appMode } ! []

        UpdateScanSubmission scanMsg ->
            let
                ( updatedScanModel, scanSubCmds ) =
                    updateScanInput scanMsg model.alanineScanModel
            in
                { model
                    | alanineScanModel =
                        updatedScanModel
                }
                    ! [ Cmd.map UpdateScanSubmission scanSubCmds ]


type ScanMsg
    = GetInputPDB
    | ProcessPDBInput String
    | ProcessStructureInfo (List ChainID)
    | SetChainVisibility ChainID Bool
    | SetLigand ChainID
    | ClearLigand
    | SubmitScanJob
    | ScanJobSubmitted (Result Http.Error String)


updateScanInput : ScanMsg -> AlanineScanModel -> ( AlanineScanModel, Cmd ScanMsg )
updateScanInput scanMsg scanModel =
    let
        scanSub =
            scanModel.alanineScanSub
    in
        case scanMsg of
            GetInputPDB ->
                scanModel ! [ requestPDBFile () ]

            ProcessPDBInput pdbString ->
                case pdbString of
                    "Failed to find file." ->
                        scanModel ! []

                    _ ->
                        { scanModel
                            | alanineScanSub =
                                { emptyScanSub
                                    | pdbFile = Just pdbString
                                    , chainVisibility = Nothing
                                    , ligand = Nothing
                                }
                        }
                            ! [ showStructure pdbString ]

            ProcessStructureInfo chainVisibility ->
                { scanModel
                    | alanineScanSub =
                        { scanSub
                            | chainVisibility =
                                List.map (\l -> ( l, True )) chainVisibility
                                    |> Dict.fromList
                                    |> Just
                        }
                }
                    ! []

            SetChainVisibility label visible ->
                case scanSub.chainVisibility of
                    Just chainVisibility ->
                        { scanModel
                            | alanineScanSub =
                                { scanSub
                                    | chainVisibility =
                                        Dict.insert label visible chainVisibility
                                            |> Just
                                }
                        }
                            ! [ setChainVisibility ( label, visible ) ]

                    Nothing ->
                        scanModel ! []

            SetLigand chainID ->
                case scanSub.chainVisibility of
                    Just chainVis ->
                        { scanModel
                            | alanineScanSub =
                                { scanSub
                                    | ligand = Just chainID
                                }
                        }
                            ! [ colourByLigand ( (Dict.keys chainVis), chainID ) ]

                    Nothing ->
                        scanModel ! []

            ClearLigand ->
                case scanSub.pdbFile of
                    Just pdb ->
                        { scanModel
                            | alanineScanSub =
                                { scanSub
                                    | ligand = Nothing
                                }
                        }
                            ! [ showStructure pdb ]

                    Nothing ->
                        scanModel ! []

            SubmitScanJob ->
                scanModel ! [ submitAlanineScan scanModel.alanineScanSub ]

            ScanJobSubmitted (Ok jobID) ->
                { scanModel
                    | alanineScanSub = emptyScanSub
                    , alanineScanJobs = jobID :: scanModel.alanineScanJobs
                }
                    ! []

            ScanJobSubmitted (Err error) ->
                scanModel ! []



-- Cmds


port initialiseViewer : () -> Cmd msg


port showStructure : String -> Cmd msg


port requestPDBFile : () -> Cmd msg


port setChainVisibility : ( ChainID, Bool ) -> Cmd msg


port colourByLigand : ( List ChainID, ChainID ) -> Cmd msg


submitAlanineScan : AlanineScanSub -> Cmd ScanMsg
submitAlanineScan scanSub =
    Http.send ScanJobSubmitted <|
        Http.post
            "/api/v0.1/alanine-scan-jobs"
            (encodeAlanineScanSub scanSub
                |> Http.jsonBody
            )
            JDe.string



-- Subscriptions


port receivePDBFile : (String -> msg) -> Sub msg


port receiveStructureInfo : (List ChainID -> msg) -> Sub msg


port atomClick : (ResidueInfo -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receivePDBFile <| UpdateScanSubmission << ProcessPDBInput
        , receiveStructureInfo <| UpdateScanSubmission << ProcessStructureInfo

        -- , atomClick <| UpdateScanSubmission << ToggleResidueSelection
        ]


residueInfoDecoder : JDe.Decoder ResidueInfo
residueInfoDecoder =
    JDe.succeed ResidueInfo
        |: (JDe.field "chainID" JDe.string)
        |: (JDe.field "aminoAcid" JDe.string)
        |: (JDe.field "residueNumber" JDe.int)



-- View


view : Model -> Html Msg
view model =
    div [ class "main-grid" ]
        [ div
            [ id "viewer" ]
            []
        , div
            [ class "control-panel" ]
            (case model.appMode of
                ScanSubmission ->
                    scanSubmissionView
                        UpdateScanSubmission
                        model.alanineScanModel.alanineScanSub
            )
        ]


scanSubmissionView : (ScanMsg -> msg) -> AlanineScanSub -> List (Html msg)
scanSubmissionView updateMsg scanSub =
    [ h2 [] [ text "Scan Submission" ]
    , div []
        [ input [ type_ "file", id "pdbFileToLoad" ] []
        , button [ onClick <| updateMsg GetInputPDB ] [ text "Upload" ]
        ]
    , div []
        (case scanSub.chainVisibility of
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
                                |> List.map
                                    (chainSelect updateMsg
                                        scanSub.ligand
                                    )
                           )
                    )
                , button
                    [ onClick <| updateMsg SubmitScanJob
                    , validScanSub scanSub |> not |> disabled
                    ]
                    [ text "Submit" ]
                ]

            Nothing ->
                []
        )
    ]


chainSelect : (ScanMsg -> msg) -> Maybe ChainID -> ( ChainID, Bool ) -> Html msg
chainSelect updateMsg mLigandLabel ( label, visible ) =
    tr []
        [ td [] [ text label ]
        , td []
            [ input
                [ onCheck <| updateMsg << (SetChainVisibility label)
                , type_ "checkbox"
                , checked visible
                ]
                []
            ]
        , td []
            (case mLigandLabel of
                Just ligandLabel ->
                    if ligandLabel == label then
                        [ button [ onClick <| updateMsg ClearLigand ]
                            [ text "Clear" ]
                        ]
                    else
                        []

                Nothing ->
                    [ button [ onClick <| updateMsg <| SetLigand label ]
                        [ text "Set" ]
                    ]
            )
        ]
