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
    , notifications : List Notification
    , notificationsOpen : Bool
    }


emptyModel : Model
emptyModel =
    Model
        ScanSubmission
        emptyAlaScanModel
        []
        False


type AppMode
    = ScanSubmission
    | ConstellationSubmission
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



-- Init


init : ( Model, Cmd msg )
init =
    emptyModel ! [ initialiseViewer () ]



-- Update


type Msg
    = SetAppMode AppMode
    | UpdateScanSubmission ScanMsg
    | ToggleNotificationPanel
    | DismissNotification Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetAppMode appMode ->
            { model | appMode = appMode } ! []

        UpdateScanSubmission scanMsg ->
            let
                ( updatedScanModel, scanSubCmds, notifications ) =
                    updateScanInput scanMsg model.alanineScan
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
                            ! [ Cmd.map UpdateScanSubmission scanSubCmds ]

                    ProcessScanResults (Ok _) ->
                        { model
                            | alanineScan =
                                updatedScanModel
                            , appMode = ScanSubmission
                            , notifications =
                                List.append
                                    model.notifications
                                    notifications
                        }
                            ! [ Cmd.map UpdateScanSubmission scanSubCmds ]

                    _ ->
                        { model
                            | alanineScan =
                                updatedScanModel
                            , notifications =
                                List.append
                                    model.notifications
                                    notifications
                        }
                            ! [ Cmd.map UpdateScanSubmission scanSubCmds ]

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


updateScanInput :
    ScanMsg
    -> AlanineScanModel
    -> ( AlanineScanModel, Cmd ScanMsg, List Notification )
updateScanInput scanMsg scanModel =
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



-- Subscriptions


port receivePDBFile : (String -> msg) -> Sub msg


port receiveStructureInfo : (List ChainID -> msg) -> Sub msg


port atomClick : (ResidueInfo -> msg) -> Sub msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ receivePDBFile <| UpdateScanSubmission << ProcessPDBInput
        , receiveStructureInfo <| UpdateScanSubmission << ProcessStructureInfo

        -- , atomClick <| UpdateScanSubmission << ToggleResidueSelection
        ]
            ++ (if List.length (getActiveJobs model.alanineScan.jobs) > 0 then
                    [ Time.every (5 * Time.second)
                        (UpdateScanSubmission << CheckScanJobs)
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
                , onClick <| SetAppMode ScanSubmission
                ]
                [ text "Scan" ]
            , div
                [ class "tab constellation-tab"
                , onClick <|
                    SetAppMode ConstellationSubmission
                ]
                [ text "Constellation" ]
            , div
                [ class "tab jobs-tab"
                , onClick <| SetAppMode Jobs
                ]
                [ text "Jobs" ]
            ]
        , (case model.appMode of
            ScanSubmission ->
                case model.alanineScan.results of
                    Just results ->
                        scanResultsView UpdateScanSubmission results

                    Nothing ->
                        scanSubmissionView
                            UpdateScanSubmission
                            model.alanineScan.alanineScanSub

            ConstellationSubmission ->
                div [ class "control-panel constellation-panel" ]
                    [ h2 [] [ text "Constellation Submission" ] ]

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
    in
        div [ class "control-panel jobs-panel" ]
            [ h2 [] [ text "Jobs" ]
            , jobTable "Alanine Scan Jobs" alaScanJobs
            ]


jobTable : String -> List JobDetails -> Html Msg
jobTable tableTitle jobs =
    let
        tableRow { jobID, status } =
            tr [ class "details" ]
                [ td [] [ text jobID ]
                , td [] [ text <| toString status ]
                , td []
                    [ button
                        [ GetScanResults jobID
                            |> UpdateScanSubmission
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
