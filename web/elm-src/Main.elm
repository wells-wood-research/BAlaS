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
import Notifications exposing ((#), Notification)
import Time


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
    , alanineScan : AlanineScanModel
    , constellation : ConstellationModel
    , notifications : List Notification
    , notificationsOpen : Bool
    }


emptyModel : Model
emptyModel =
    Model
        Scan
        emptyAlaScanModel
        emptyConstellationModel
        []
        False


type AppMode
    = Scan
    | Constellation
    | Jobs


type alias JobDetails =
    { jobID : String
    , status : JobStatus
    }


jobDetailsDecoder : JDe.Decoder JobDetails
jobDetailsDecoder =
    JDe.succeed JobDetails
        |: (JDe.field "_id" JDe.string)
        |: (JDe.field "status" (JDe.int |> JDe.andThen intToJobStatus))


type JobStatus
    = Submitted
    | Queued
    | Running
    | Completed
    | Failed


intToJobStatus : Int -> JDe.Decoder JobStatus
intToJobStatus statusNumber =
    case statusNumber of
        1 ->
            JDe.succeed Submitted

        2 ->
            JDe.succeed Queued

        3 ->
            JDe.succeed Running

        4 ->
            JDe.succeed Completed

        5 ->
            JDe.succeed Failed

        _ ->
            JDe.fail "Unknown status."


type alias ChainID =
    String


type alias AlanineScanModel =
    { alanineScanSub : AlanineScanSub
    , jobs : List JobDetails
    , results : Maybe AlanineScanResults
    }


emptyAlaScanModel : AlanineScanModel
emptyAlaScanModel =
    AlanineScanModel emptyScanSub [] Nothing


type alias AlanineScanSub =
    { pdbFile : Maybe String
    , chainVisibility : Maybe (Dict.Dict ChainID Bool)
    , receptor : List ChainID
    , ligand : List ChainID
    }


validScanSub : AlanineScanSub -> Bool
validScanSub { pdbFile, receptor, ligand } =
    let
        isJust mA =
            case mA of
                Just _ ->
                    True

                Nothing ->
                    False
    in
        isJust pdbFile && List.length receptor > 0 && List.length ligand > 0


emptyScanSub : AlanineScanSub
emptyScanSub =
    AlanineScanSub
        Nothing
        Nothing
        []
        []


encodeAlanineScanSub : AlanineScanSub -> JEn.Value
encodeAlanineScanSub scanSub =
    JEn.object
        [ ( "pdbFile", Maybe.withDefault "Invalid" scanSub.pdbFile |> JEn.string )
        , ( "receptor", List.map JEn.string scanSub.receptor |> JEn.list )
        , ( "ligand", List.map JEn.string scanSub.ligand |> JEn.list )
        ]


type alias AlanineScanResults =
    { pdbFile : String
    , receptor : List ChainID
    , ligand : List ChainID
    , dG : Float
    , receptorResults : List ResidueResult
    , ligandResults : List ResidueResult
    }


type alias ResidueResult =
    { residueNumber : String
    , aminoAcid : String
    , chainID : ChainID
    , ddG : Float
    , residueSize : Int
    , stdDevDDG : Float
    }


scanResultsDecoder : JDe.Decoder AlanineScanResults
scanResultsDecoder =
    JDe.succeed AlanineScanResults
        |: (JDe.field "pdbFile" JDe.string)
        |: (JDe.field "receptor" (JDe.list JDe.string))
        |: (JDe.field "ligand" (JDe.list JDe.string))
        |: (JDe.field "dG" JDe.float)
        |: (JDe.field "receptorData" <|
                JDe.list <|
                    JDe.succeed ResidueResult
                        |: (JDe.index 0 JDe.string)
                        |: (JDe.index 1 JDe.string)
                        |: (JDe.index 2 JDe.string)
                        |: (JDe.index 3 JDe.float)
                        |: (JDe.index 4 JDe.int)
                        |: (JDe.index 5 JDe.float)
           )
        |: (JDe.field "ligandData" <|
                JDe.list <|
                    JDe.succeed ResidueResult
                        |: (JDe.index 0 JDe.string)
                        |: (JDe.index 1 JDe.string)
                        |: (JDe.index 2 JDe.string)
                        |: (JDe.index 3 JDe.float)
                        |: (JDe.index 4 JDe.int)
                        |: (JDe.index 5 JDe.float)
           )


type alias ResidueInfo =
    { chainID : ChainID
    , aminoAcid : String
    , residueNumber : Int
    }


type alias ConstellationModel =
    { constellationSub : ConstellationMode
    , jobs : List JobDetails
    , results : Maybe ConstellationResults
    }


emptyConstellationModel : ConstellationModel
emptyConstellationModel =
    ConstellationModel
        (Auto defaultAutoSettings)
        []
        Nothing


type ConstellationMode
    = Auto AutoSettings


type alias AutoSettings =
    { ddGCutOff : String
    , constellationSize : String
    , cutOffDistance : String
    }


defaultAutoSettings : AutoSettings
defaultAutoSettings =
    AutoSettings
        "5.0"
        "3"
        "13.0"


validAutoSettings : AutoSettings -> Bool
validAutoSettings { ddGCutOff, constellationSize, cutOffDistance } =
    let
        validDDGCutOff =
            representsFloat ddGCutOff

        validConSize =
            representsInt constellationSize

        validDistance =
            representsFloat cutOffDistance

        anyEmpty =
            List.map
                String.isEmpty
                [ ddGCutOff, constellationSize, cutOffDistance ]
                |> List.any identity
    in
        List.all identity
            [ validDDGCutOff
            , validConSize
            , validDistance
            , not anyEmpty
            ]


encodeAutoJob : AlanineScanResults -> AutoSettings -> JEn.Value
encodeAutoJob scanResults settings =
    JEn.object
        [ ( "ddGCutOff"
          , Result.withDefault
                5.0
                (String.toFloat
                    settings.ddGCutOff
                )
                |> JEn.float
          )
        , ( "constellationSize"
          , Result.withDefault
                3
                (String.toInt
                    settings.constellationSize
                )
                |> JEn.int
          )
        , ( "cutOffDistance"
          , Result.withDefault
                13.0
                (String.toFloat
                    settings.ddGCutOff
                )
                |> JEn.float
          )
        , ( "pdbFile", scanResults.pdbFile |> JEn.string )
        , ( "receptor", List.map JEn.string scanResults.receptor |> JEn.list )
        , ( "ligand", List.map JEn.string scanResults.ligand |> JEn.list )
        ]


representsFloat : String -> Bool
representsFloat inString =
    case String.toFloat inString of
        Ok _ ->
            True

        Err _ ->
            False


representsInt : String -> Bool
representsInt inString =
    case String.toInt inString of
        Ok _ ->
            True

        Err _ ->
            False


type alias ConstellationResults =
    { hotConstellations : List ( String, Float )
    , scanResults : AlanineScanResults
    }


autoResultsDecoder : JDe.Decoder ConstellationResults
autoResultsDecoder =
    JDe.succeed ConstellationResults
        |: (JDe.field "hotConstellations" <|
                JDe.list <|
                    JDe.map2
                        (,)
                        (JDe.index 0 JDe.string)
                        (JDe.index 1 JDe.float)
           )
        |: (JDe.field "scanResults" scanResultsDecoder)



-- Init


init : ( Model, Cmd msg )
init =
    emptyModel ! [ initialiseViewer () ]



-- Update


type Msg
    = SetAppMode AppMode
    | UpdateScan ScanMsg
    | UpdateConstellation ConstellationMsg
    | ToggleNotificationPanel
    | DismissNotification Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAppMode appMode ->
            { model | appMode = appMode } ! []

        UpdateScan scanMsg ->
            let
                ( updatedScanModel, scanSubCmds, notifications ) =
                    updateScan scanMsg model.alanineScan
            in
                case scanMsg of
                    ScanJobSubmitted (Ok _) ->
                        { model
                            | alanineScan =
                                updatedScanModel
                            , appMode = Jobs
                            , notifications =
                                List.append
                                    model.notifications
                                    notifications
                        }
                            ! [ Cmd.map UpdateScan scanSubCmds ]

                    ProcessScanResults (Ok _) ->
                        { model
                            | alanineScan =
                                updatedScanModel
                            , appMode = Scan
                            , notifications =
                                List.append
                                    model.notifications
                                    notifications
                        }
                            ! [ Cmd.map UpdateScan scanSubCmds ]

                    _ ->
                        { model
                            | alanineScan =
                                updatedScanModel
                            , notifications =
                                List.append
                                    model.notifications
                                    notifications
                        }
                            ! [ Cmd.map UpdateScan scanSubCmds ]

        UpdateConstellation constellationMsg ->
            let
                ( updatedConModel, conSubCmds, notifications ) =
                    updateConstellation constellationMsg model.constellation
            in
                { model
                    | constellation =
                        updatedConModel
                    , notifications =
                        List.append
                            model.notifications
                            notifications
                }
                    ! [ Cmd.map UpdateConstellation conSubCmds ]

        ToggleNotificationPanel ->
            { model | notificationsOpen = not model.notificationsOpen }
                ! []

        DismissNotification idx ->
            { model
                | notifications =
                    removeListItem idx
                        model.notifications
            }
                ! []


removeListItem : Int -> List a -> List a
removeListItem idx editList =
    List.indexedMap (,) editList
        |> List.filter (\( refIdx, _ ) -> refIdx /= idx)
        |> List.map Tuple.second


type ScanMsg
    = GetInputPDB
    | ProcessPDBInput String
    | ProcessStructureInfo (List ChainID)
    | SetChainVisibility ChainID Bool
    | SetReceptor ChainID
    | SetLigand ChainID
    | ClearReceptor ChainID
    | ClearLigand ChainID
    | SubmitScanJob
    | ScanJobSubmitted (Result Http.Error JobDetails)
    | CheckScanJobs Time.Time
    | ProcessScanStatus (Result Http.Error JobDetails)
    | GetScanResults String
    | ProcessScanResults (Result Http.Error AlanineScanResults)
    | ColourByDDG
    | ClearScanSubmission


updateScan :
    ScanMsg
    -> AlanineScanModel
    -> ( AlanineScanModel, Cmd ScanMsg, List Notification )
updateScan scanMsg scanModel =
    let
        scanSub =
            scanModel.alanineScanSub
    in
        case scanMsg of
            GetInputPDB ->
                scanModel ! [ requestPDBFile () ] # []

            ProcessPDBInput pdbString ->
                case pdbString of
                    "Failed to find file." ->
                        scanModel ! [] # []

                    _ ->
                        { scanModel
                            | alanineScanSub =
                                { emptyScanSub
                                    | pdbFile = Just pdbString
                                    , chainVisibility = Nothing
                                    , receptor = []
                                    , ligand = []
                                }
                        }
                            ! [ showStructure pdbString ]
                            # []

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
                    # []

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
                            # []

                    Nothing ->
                        scanModel ! [] # []

            SetReceptor chainID ->
                case scanSub.chainVisibility of
                    Just chainVis ->
                        { scanModel
                            | alanineScanSub =
                                { scanSub
                                    | receptor =
                                        List.append
                                            scanSub.receptor
                                            [ chainID ]
                                }
                        }
                            ! [ colourGeometry ( "red", chainID ) ]
                            # []

                    Nothing ->
                        scanModel ! [] # []

            SetLigand chainID ->
                case scanSub.chainVisibility of
                    Just chainVis ->
                        { scanModel
                            | alanineScanSub =
                                { scanSub
                                    | ligand =
                                        List.append
                                            scanSub.ligand
                                            [ chainID ]
                                }
                        }
                            ! [ colourGeometry ( "blue", chainID ) ]
                            # []

                    Nothing ->
                        scanModel ! [] # []

            ClearReceptor chainID ->
                case scanSub.pdbFile of
                    Just pdb ->
                        { scanModel
                            | alanineScanSub =
                                { scanSub
                                    | receptor =
                                        List.filter
                                            (\r -> r /= chainID)
                                            scanSub.receptor
                                }
                        }
                            ! [ colourGeometry ( "white", chainID ) ]
                            # []

                    Nothing ->
                        scanModel ! [] # []

            ClearLigand chainID ->
                case scanSub.pdbFile of
                    Just pdb ->
                        { scanModel
                            | alanineScanSub =
                                { scanSub
                                    | ligand =
                                        List.filter
                                            (\l -> l /= chainID)
                                            scanSub.ligand
                                }
                        }
                            ! [ colourGeometry ( "white", chainID ) ]
                            # []

                    Nothing ->
                        scanModel ! [] # []

            SubmitScanJob ->
                scanModel ! [ submitAlanineScan scanModel.alanineScanSub ] # []

            ScanJobSubmitted (Ok jobDetails) ->
                { scanModel
                    | alanineScanSub = emptyScanSub
                    , jobs = jobDetails :: scanModel.jobs
                }
                    ! []
                    # []

            ScanJobSubmitted (Err error) ->
                let
                    err =
                        Debug.log "Scan job submission error" error
                in
                    scanModel
                        ! []
                        # [ Notifications.errorToNotification
                                "Failed to Submit Scan Job"
                                error
                          ]

            CheckScanJobs _ ->
                scanModel
                    ! List.map checkScanJobStatus
                        scanModel.jobs
                    # []

            ProcessScanStatus (Ok jobDetails) ->
                { scanModel
                    | jobs =
                        scanModel.jobs
                            |> List.map (\{ jobID, status } -> ( jobID, status ))
                            |> Dict.fromList
                            |> Dict.insert jobDetails.jobID jobDetails.status
                            |> Dict.toList
                            |> List.map (\( jobID, status ) -> JobDetails jobID status)
                }
                    ! []
                    # case jobDetails.status of
                        Completed ->
                            [ Notification
                                ""
                                "Alanine Scan Completed"
                                ("Alanine scan job "
                                    ++ jobDetails.jobID
                                    ++ " complete. Retrieve the results from"
                                    ++ " the 'Jobs' tab."
                                )
                            ]

                        Failed ->
                            [ Notification
                                ""
                                "Alanine Scan Failed"
                                ("Alanine scan job "
                                    ++ jobDetails.jobID
                                    ++ " failed."
                                )
                            ]

                        _ ->
                            []

            ProcessScanStatus (Err error) ->
                let
                    err =
                        Debug.log "Scan status check error" error
                in
                    scanModel ! [] # []

            GetScanResults scanID ->
                scanModel ! [ getScanResults scanID ] # []

            ProcessScanResults (Ok results) ->
                { scanModel | results = Just results }
                    ! [ colourByDDG results ]
                    # []

            ProcessScanResults (Err error) ->
                let
                    err =
                        Debug.log "Failed to retrieve scan results" error
                in
                    scanModel ! [] # []

            ColourByDDG ->
                case scanModel.results of
                    Just results ->
                        scanModel
                            ! [ colourByDDG results ]
                            # []

                    Nothing ->
                        scanModel ! [] # []

            ClearScanSubmission ->
                { scanModel | results = Nothing } ! [] # []


getActiveJobs : List JobDetails -> List JobDetails
getActiveJobs jobs =
    List.filter
        (\{ status } -> (status /= Completed) && (status /= Failed))
        jobs


type ConstellationMsg
    = ChangeMode String
    | UpdateAutoSettings AutoSettingsMsg
    | SubmitConstellationJob AlanineScanResults
    | AutoJobSubmitted (Result Http.Error JobDetails)
    | CheckAutoJobs Time.Time
    | ProcessAutoJobStatus (Result Http.Error JobDetails)
    | GetAutoResults String
    | ProcessAutoResults (Result Http.Error ConstellationResults)


updateConstellation :
    ConstellationMsg
    -> ConstellationModel
    -> ( ConstellationModel, Cmd ConstellationMsg, List Notification )
updateConstellation msg model =
    let
        conSub =
            model.constellationSub
    in
        case msg of
            ChangeMode modeString ->
                case modeString of
                    "Auto" ->
                        { model
                            | constellationSub = Auto defaultAutoSettings
                        }
                            ! []
                            # []

                    _ ->
                        model ! [] # []

            UpdateAutoSettings settingsMsg ->
                case model.constellationSub of
                    Auto settings ->
                        { model
                            | constellationSub =
                                updateAutoSettings settingsMsg settings
                                    |> Auto
                        }
                            ! []
                            # []

            SubmitConstellationJob alaScanResults ->
                case model.constellationSub of
                    Auto settings ->
                        model ! [ submitAutoJob alaScanResults settings ] # []

            AutoJobSubmitted (Ok jobDetails) ->
                { model
                    | constellationSub = Auto defaultAutoSettings
                    , jobs = jobDetails :: model.jobs
                }
                    ! []
                    # []

            AutoJobSubmitted (Err error) ->
                let
                    err =
                        Debug.log "Auto job submission error" error
                in
                    model
                        ! []
                        # [ Notifications.errorToNotification
                                "Failed to Constellation Scan Auto Job"
                                error
                          ]

            CheckAutoJobs _ ->
                model
                    ! List.map checkAutoJobStatus
                        model.jobs
                    # []

            ProcessAutoJobStatus (Ok jobDetails) ->
                { model
                    | jobs =
                        model.jobs
                            |> List.map (\{ jobID, status } -> ( jobID, status ))
                            |> Dict.fromList
                            |> Dict.insert jobDetails.jobID jobDetails.status
                            |> Dict.toList
                            |> List.map (\( jobID, status ) -> JobDetails jobID status)
                }
                    ! []
                    # case jobDetails.status of
                        Completed ->
                            [ Notification
                                ""
                                "Auto Constellation Scan Completed"
                                ("Auto constellation scan job "
                                    ++ jobDetails.jobID
                                    ++ " complete. Retrieve the results from"
                                    ++ " the 'Jobs' tab."
                                )
                            ]

                        Failed ->
                            [ Notification
                                ""
                                "Auto Constellation Scan Failed"
                                ("Auto constellation scan job "
                                    ++ jobDetails.jobID
                                    ++ " failed."
                                )
                            ]

                        _ ->
                            []

            ProcessAutoJobStatus (Err error) ->
                let
                    err =
                        Debug.log "Auto job status check error" error
                in
                    model ! [] # []

            GetAutoResults jobID ->
                model ! [ getAutoResults jobID ] # []

            ProcessAutoResults (Ok results) ->
                { model | results = Just results }
                    ! []
                    # []

            ProcessAutoResults (Err error) ->
                let
                    err =
                        Debug.log "Failed to retrieve scan results" error
                in
                    model ! [] # []


type AutoSettingsMsg
    = UpdateDDGCutOff String
    | UpdateSize String
    | UpdateDistanceCutOff String


updateAutoSettings : AutoSettingsMsg -> AutoSettings -> AutoSettings
updateAutoSettings msg settings =
    case msg of
        UpdateDDGCutOff ddGValue ->
            { settings | ddGCutOff = ddGValue }

        UpdateSize constellationSize ->
            { settings | constellationSize = constellationSize }

        UpdateDistanceCutOff distance ->
            { settings | cutOffDistance = distance }



-- Cmds


port initialiseViewer : () -> Cmd msg


port showStructure : String -> Cmd msg


port requestPDBFile : () -> Cmd msg


port setChainVisibility : ( ChainID, Bool ) -> Cmd msg


port colourGeometry : ( String, ChainID ) -> Cmd msg


port colourByDDG : AlanineScanResults -> Cmd msg


submitAlanineScan : AlanineScanSub -> Cmd ScanMsg
submitAlanineScan scanSub =
    Http.send ScanJobSubmitted <|
        Http.post
            "/api/v0.1/alanine-scan-jobs"
            (encodeAlanineScanSub scanSub
                |> Http.jsonBody
            )
            jobDetailsDecoder


checkScanJobStatus : JobDetails -> Cmd ScanMsg
checkScanJobStatus { jobID } =
    Http.send ProcessScanStatus <|
        Http.get
            ("/api/v0.1/alanine-scan-job/" ++ jobID ++ "?get-status")
            jobDetailsDecoder


getScanResults : String -> Cmd ScanMsg
getScanResults jobID =
    Http.send ProcessScanResults <|
        Http.get
            ("/api/v0.1/alanine-scan-job/" ++ jobID ++ "?get-results")
            scanResultsDecoder


submitAutoJob : AlanineScanResults -> AutoSettings -> Cmd ConstellationMsg
submitAutoJob scanResults settings =
    Http.send AutoJobSubmitted <|
        Http.post
            "/api/v0.1/auto-jobs"
            (encodeAutoJob scanResults settings
                |> Http.jsonBody
            )
            jobDetailsDecoder


checkAutoJobStatus : JobDetails -> Cmd ConstellationMsg
checkAutoJobStatus { jobID } =
    Http.send ProcessAutoJobStatus <|
        Http.get
            ("/api/v0.1/auto-job/" ++ jobID ++ "?get-status")
            jobDetailsDecoder


getAutoResults : String -> Cmd ConstellationMsg
getAutoResults jobID =
    Http.send ProcessAutoResults <|
        Http.get
            ("/api/v0.1/auto-job/" ++ jobID ++ "?get-results")
            autoResultsDecoder



-- Subscriptions


port receivePDBFile : (String -> msg) -> Sub msg


port receiveStructureInfo : (List ChainID -> msg) -> Sub msg


port atomClick : (ResidueInfo -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ receivePDBFile <| UpdateScan << ProcessPDBInput
        , receiveStructureInfo <| UpdateScan << ProcessStructureInfo

        -- , atomClick <| UpdateScan << ToggleResidueSelection
        ]
            ++ (if List.length (getActiveJobs model.alanineScan.jobs) > 0 then
                    [ Time.every (5 * Time.second)
                        (UpdateScan << CheckScanJobs)
                    ]
                else
                    []
               )
            ++ (if List.length (getActiveJobs model.constellation.jobs) > 0 then
                    [ Time.every (5 * Time.second)
                        (UpdateConstellation << CheckAutoJobs)
                    ]
                else
                    []
               )


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
        [ header
            [ class "title-bar" ]
            [ h1 [] [ text "BUDE Alanine Scan" ] ]
        , notificationPanel model.notificationsOpen model.notifications
        , div
            [ class "notifications-button"
            , onClick <| ToggleNotificationPanel
            ]
            [ List.length model.notifications |> toString |> (++) "🔔" |> text ]
        , div
            [ id "viewer" ]
            []
        , div [ class "tabs" ]
            [ div
                [ class "tab scan-tab"
                , onClick <| SetAppMode Scan
                ]
                [ text "Scan" ]
            , div
                [ class "tab constellation-tab"
                , onClick <|
                    SetAppMode Constellation
                ]
                [ text "Constellation" ]
            , div
                [ class "tab jobs-tab"
                , onClick <| SetAppMode Jobs
                ]
                [ text "Jobs" ]
            ]
        , (case model.appMode of
            Scan ->
                case model.alanineScan.results of
                    Just results ->
                        scanResultsView UpdateScan results

                    Nothing ->
                        scanSubmissionView
                            UpdateScan
                            model.alanineScan.alanineScanSub

            Constellation ->
                case model.constellation.results of
                    Just results ->
                        div [] [ text "results" ]

                    Nothing ->
                        constellationSubmissionView
                            UpdateConstellation
                            model.constellation
                            model.alanineScan.results

            Jobs ->
                jobsView model
          )
        ]


notificationPanel : Bool -> List Notification -> Html Msg
notificationPanel open notifications =
    div [ class "notification-panel", hidden <| not open ]
        [ h3 [] [ text "Notifications" ]
        , button [ onClick ToggleNotificationPanel ] [ text "Close" ]
        , div [] <| List.indexedMap notificationView notifications
        ]


notificationView : Int -> Notification -> Html Msg
notificationView idx notification =
    div [ class "notification" ]
        [ h4 [] [ text notification.title ]
        , button [ onClick <| DismissNotification idx ] [ text "Dismiss" ]
        , p [ class "details" ]
            [ text notification.message ]
        ]


scanSubmissionView : (ScanMsg -> msg) -> AlanineScanSub -> Html msg
scanSubmissionView updateMsg scanSub =
    div
        [ class "control-panel scan-panel" ]
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
                            , th [] [ text "Receptor" ]
                            , th [] [ text "Ligand" ]
                            ]
                         ]
                            ++ (Dict.toList chainVisibility
                                    |> List.map
                                        (chainSelect updateMsg
                                            scanSub.receptor
                                            scanSub.ligand
                                        )
                               )
                        )
                    , button
                        [ onClick <| updateMsg SubmitScanJob
                        , validScanSub scanSub |> not |> disabled
                        ]
                        [ text "Start Scan" ]
                    ]

                Nothing ->
                    []
            )
        ]


scanResultsView : (ScanMsg -> msg) -> AlanineScanResults -> Html msg
scanResultsView updateMsg results =
    let
        resultsRow { chainID, residueNumber, aminoAcid, ddG } =
            tr []
                [ td [] [ text chainID ]
                , td [] [ text residueNumber ]
                , td [] [ text aminoAcid ]
                , td [] [ toString ddG |> text ]
                ]
    in
        div
            [ class "control-panel scan-panel" ]
            [ h2 [] [ text "Alanine Scan Results" ]
            , button
                [ onClick <| updateMsg ClearScanSubmission ]
                [ text "New Submission" ]
            , h3 [] [ text "ΔG" ]
            , p [] [ toString results.dG |> text ]
            , h3 [] [ text "Residue Results (Non-Zero)" ]
            , div [ class "scan-results-table" ]
                [ table []
                    ([ tr []
                        [ th [] [ text "Chain" ]
                        , th [] [ text "Residue" ]
                        , th [] [ text "Amino Acid" ]
                        , th [] [ text "ΔΔG" ]
                        ]
                     ]
                        ++ (List.filter (\res -> res.ddG /= 0) results.ligandResults
                                |> List.map resultsRow
                           )
                    )
                ]
            ]


chainSelect :
    (ScanMsg -> msg)
    -> List ChainID
    -> List ChainID
    -> ( ChainID, Bool )
    -> Html msg
chainSelect updateMsg receptorLabels ligandLabels ( label, visible ) =
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
            (if List.member label receptorLabels then
                [ button
                    [ onClick <|
                        updateMsg <|
                            ClearReceptor label
                    ]
                    [ text "Clear" ]
                ]
             else if List.member label ligandLabels then
                []
             else
                [ button
                    [ onClick <|
                        updateMsg <|
                            SetReceptor label
                    ]
                    [ text "Set" ]
                ]
            )
        , td []
            (if List.member label ligandLabels then
                [ button
                    [ onClick <|
                        updateMsg <|
                            ClearLigand label
                    ]
                    [ text "Clear" ]
                ]
             else if List.member label receptorLabels then
                []
             else
                [ button
                    [ onClick <|
                        updateMsg <|
                            SetLigand label
                    ]
                    [ text "Set" ]
                ]
            )
        ]


jobsView : Model -> Html Msg
jobsView model =
    let
        alaScanJobs =
            model.alanineScan.jobs

        constellationJobs =
            model.constellation.jobs
    in
        div [ class "control-panel jobs-panel" ]
            [ h2 [] [ text "Jobs" ]
            , jobTable (UpdateScan << GetScanResults)
                "Alanine Scan Jobs"
                alaScanJobs
            , jobTable (UpdateConstellation << GetAutoResults)
                "Constellation Scan Jobs"
                constellationJobs
            ]


jobTable : (String -> Msg) -> String -> List JobDetails -> Html Msg
jobTable getMsg tableTitle jobs =
    let
        tableRow { jobID, status } =
            tr [ class "details" ]
                [ td [] [ text jobID ]
                , td [] [ text <| toString status ]
                , td []
                    [ button
                        [ getMsg jobID
                            |> onClick
                        , (if (status == Completed) then
                            False
                           else
                            True
                          )
                            |> disabled
                        ]
                        [ text "Get Results" ]
                    ]
                ]
    in
        div []
            [ h3 [] [ text tableTitle ]
            , if List.length jobs > 0 then
                table []
                    ([ tr []
                        [ th [] [ text "Job ID" ]
                        , th [] [ text "Status" ]
                        ]
                     ]
                        ++ List.map tableRow jobs
                    )
              else
                text "No Jobs submitted."
            ]



-- Constellations


constellationSubmissionView :
    (ConstellationMsg -> msg)
    -> ConstellationModel
    -> Maybe AlanineScanResults
    -> Html msg
constellationSubmissionView updateMsg model mScanResults =
    div [ class "control-panel constellation-panel" ]
        [ h2 [] [ text "Constellation Submission" ]
        , case mScanResults of
            Just results ->
                activeConstellationSub updateMsg model results

            Nothing ->
                div []
                    [ text
                        ("You must run a scan job before you run a constellation"
                            ++ " scan. If you've already ran a scan job, reload the"
                            ++ " results from the jobs tab."
                        )
                    ]
        ]


activeConstellationSub :
    (ConstellationMsg -> msg)
    -> ConstellationModel
    -> AlanineScanResults
    -> Html msg
activeConstellationSub updateMsg model scanRes =
    div []
        [ text "Select Mode"
        , br [] []
        , select [ onInput <| updateMsg << ChangeMode ] <|
            List.map simpleOption [ "Auto" ]
        , case model.constellationSub of
            Auto settings ->
                autoSettingsView updateMsg scanRes settings
        ]


simpleOption : String -> Html msg
simpleOption labelValue =
    option [ value labelValue ] [ text labelValue ]


autoSettingsView :
    (ConstellationMsg -> msg)
    -> AlanineScanResults
    -> AutoSettings
    -> Html msg
autoSettingsView updateMsg scanRes settings =
    let
        { ddGCutOff, constellationSize, cutOffDistance } =
            settings
    in
        div []
            ([ text "ΔΔG Cut Off Value"
             , input
                [ onInput <| updateMsg << UpdateAutoSettings << UpdateDDGCutOff
                , pattern "[+-]?([0-9]*[.])?[0-9]+"
                , value ddGCutOff
                , placeholder "ΔΔG"
                ]
                []
             , text "Constellation Size"
             , input
                [ onInput <| updateMsg << UpdateAutoSettings << UpdateSize
                , pattern "[0-9]*"
                , value constellationSize
                , placeholder "Size"
                ]
                []
             , text "Cut Off Distance"
             , input
                [ onInput <| updateMsg << UpdateAutoSettings << UpdateDistanceCutOff
                , pattern "[+-]?([0-9]*[.])?[0-9]+"
                , value cutOffDistance
                , placeholder "Distance"
                ]
                []
             , button
                [ onClick <| updateMsg <| SubmitConstellationJob scanRes
                , disabled <| not <| validAutoSettings settings
                ]
                [ text "Submit" ]
             ]
                |> List.intersperse (br [] [])
            )
