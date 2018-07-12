module Update exposing (..)

{- # UPDATE
   This section contains all code that updates the state of the application.
-}

import Dict
import Http
import Model
import Notifications exposing ((#), Notification)
import Ports
import Set
import Time


{-| Type that signals to the main update function how the model should be
updated.
-}
type Msg
    = SetAppMode Model.AppMode
    | UpdateScan ScanMsg
    | UpdateConstellation ConstellationMsg
    | SetHoveredName (Maybe String)
    | DismissNotification Int
    | ClearNotifications
    | OpenPanel Model.Panel
    | ClosePanel
    | SaveState


{-| The main update function that is run whenever a user interacts with an
element of the application.

The main update function handles all `Msgs` that are received, but be aware that
it is also responsive to certain `ScanMsgs` and `ConstellationMsgs`. For example
when a `ClearScanSubmission` is received, this is processed mainly through
`updatedScan`, but `update` also modifies the `mode` field of the model and
clears the current constellation results.

-}
update : Msg -> Model.Model -> ( Model.Model, Cmd Msg )
update msg model =
    case msg of
        SetAppMode appMode ->
            { model | appMode = appMode } ! []

        UpdateScan scanMsg ->
            let
                constellation =
                    model.constellation

                ( updatedScanModel, scanSubCmds, notifications ) =
                    updateScan scanMsg model.alanineScan
            in
                case scanMsg of
                    ClearScanSubmission ->
                        { model
                            | alanineScan =
                                updatedScanModel
                            , constellation =
                                { constellation
                                    | results =
                                        Nothing
                                }
                            , notifications =
                                List.filter
                                    (\n ->
                                        List.member n
                                            model.notifications
                                            |> not
                                    )
                                    notifications
                                    |> List.append
                                        model.notifications
                        }
                            ! [ Cmd.map UpdateScan scanSubCmds ]

                    ScanJobSubmitted (Ok _) ->
                        let
                            ( updatedModel, updatedCmds ) =
                                { model
                                    | alanineScan =
                                        updatedScanModel
                                    , appMode = Model.Jobs
                                    , notifications =
                                        List.filter
                                            (\n ->
                                                List.member n
                                                    model.notifications
                                                    |> not
                                            )
                                            notifications
                                            |> List.append
                                                model.notifications
                                }
                                    |> update SaveState
                        in
                            updatedModel
                                ! [ Cmd.map UpdateScan scanSubCmds
                                  , updatedCmds
                                  ]

                    ProcessScanResults (Ok _) ->
                        { model
                            | alanineScan =
                                updatedScanModel
                            , constellation =
                                { constellation
                                    | results =
                                        Nothing
                                }
                            , appMode = Model.Scan
                            , notifications =
                                List.filter
                                    (\n ->
                                        List.member n
                                            model.notifications
                                            |> not
                                    )
                                    notifications
                                    |> List.append
                                        model.notifications
                        }
                            ! [ Cmd.map UpdateScan scanSubCmds ]

                    _ ->
                        { model
                            | alanineScan =
                                updatedScanModel
                            , notifications =
                                List.filter
                                    (\n ->
                                        List.member n
                                            model.notifications
                                            |> not
                                    )
                                    notifications
                                    |> List.append
                                        model.notifications
                        }
                            ! [ Cmd.map UpdateScan scanSubCmds ]

        UpdateConstellation constellationMsg ->
            case constellationMsg of
                AutoJobSubmitted (Ok _) ->
                    sucessfulConSubmit model constellationMsg

                ProcessAutoResults (Ok { scanResults }) ->
                    sucessfulConResults model scanResults constellationMsg

                ManualJobSubmitted (Ok _) ->
                    sucessfulConSubmit model constellationMsg

                ProcessManualResults (Ok { scanResults }) ->
                    sucessfulConResults model scanResults constellationMsg

                _ ->
                    let
                        ( updatedConModel, conSubCmds, notifications ) =
                            updateConstellation constellationMsg model.constellation
                    in
                        { model
                            | constellation =
                                updatedConModel
                            , notifications =
                                List.filter
                                    (\n ->
                                        List.member n
                                            model.notifications
                                            |> not
                                    )
                                    notifications
                                    |> List.append
                                        model.notifications
                        }
                            ! [ Cmd.map UpdateConstellation conSubCmds ]

        SetHoveredName mName ->
            { model | hoveredName = mName } ! []

        DismissNotification idx ->
            { model
                | notifications =
                    removeListItem idx
                        model.notifications
            }
                ! []

        ClearNotifications ->
            { model | notifications = [] } ! []

        OpenPanel panel ->
            case model.openPanel of
                Model.Closed ->
                    { model | openPanel = panel }
                        ! []

                _ ->
                    { model | openPanel = Model.Closed }
                        ! []

        ClosePanel ->
            { model | openPanel = Model.Closed } ! []

        SaveState ->
            model ! [ Model.exportModel model |> Ports.saveState ]


sucessfulConSubmit : Model.Model -> ConstellationMsg -> ( Model.Model, Cmd Msg )
sucessfulConSubmit model constellationMsg =
    let
        ( updatedConModel, conSubCmds, notifications ) =
            updateConstellation constellationMsg model.constellation

        ( updatedModel, updatedCmds ) =
            { model
                | appMode = Model.Jobs
                , constellation =
                    updatedConModel
                , notifications =
                    List.filter
                        (\n ->
                            List.member n
                                model.notifications
                                |> not
                        )
                        notifications
                        |> List.append
                            model.notifications
            }
                |> update SaveState
    in
        updatedModel
            ! [ Cmd.map UpdateConstellation conSubCmds
              , updatedCmds
              ]


sucessfulConResults :
    Model.Model
    -> Model.AlanineScanResults
    -> ConstellationMsg
    -> ( Model.Model, Cmd Msg )
sucessfulConResults model scanResults constellationMsg =
    let
        ( updatedConModel, conSubCmds, notifications ) =
            updateConstellation constellationMsg model.constellation

        scanUpdateMsg =
            ProcessScanResults (Ok scanResults)
                |> UpdateScan

        ( updatedModel, scanCmds ) =
            update scanUpdateMsg model
    in
        { updatedModel
            | appMode = Model.Constellation
            , constellation =
                updatedConModel
            , notifications =
                List.filter
                    (\n ->
                        List.member n
                            model.notifications
                            |> not
                    )
                    notifications
                    |> List.append
                        model.notifications
        }
            ! [ Cmd.map UpdateConstellation conSubCmds
              , scanCmds
              ]



-- Scan Mode


{-| Type that signals to `updateScan` how the current `AlanineScanModel` should
be updated.
-}
type ScanMsg
    = GetStructure
    | UpdateStructure (Maybe Model.Structure)
    | ChangeVisibility String
    | SetReceptor Model.ChainID
    | SetLigand Model.ChainID
    | ClearReceptor Model.ChainID
    | ClearLigand Model.ChainID
    | SetScanName String
    | SubmitScanJob
    | ScanJobSubmitted (Result Http.Error Model.JobDetails)
    | CheckScanJobs Time.Time
    | ProcessScanStatus (Result Http.Error Model.JobDetails)
    | GetScanResults String
    | ProcessScanResults (Result Http.Error Model.AlanineScanResults)
    | DeleteScanJob String
    | FocusOnResidue Model.ResidueResult
    | ClearScanSubmission


{-| Function for updating the state of the currently active `AlanineScanModel`.
This includes code for manipulating the structure of the currently active
scan submission structure.
-}
updateScan :
    ScanMsg
    -> Model.AlanineScanModel
    -> ( Model.AlanineScanModel, Cmd ScanMsg, List Notification )
updateScan scanMsg scanModel =
    let
        scanSub =
            scanModel.alanineScanSub
    in
        case scanMsg of
            GetStructure ->
                scanModel ! [ Ports.requestPDBFile () ] # []

            UpdateStructure structure ->
                { scanModel | structure = structure } ! [] # []

            ChangeVisibility label ->
                case scanModel.structure of
                    Just structure ->
                        let
                            hiddenDict =
                                List.map2
                                    (,)
                                    structure.chainLabels
                                    structure.hidden
                                    |> Dict.fromList
                                    |> Dict.update label (Maybe.map not)
                        in
                            case Dict.get label hiddenDict of
                                Just hidden ->
                                    { scanModel
                                        | structure =
                                            Just
                                                { structure
                                                    | hidden =
                                                        Dict.values
                                                            hiddenDict
                                                }
                                    }
                                        ! [ Ports.setVisibility ( label, hidden ) ]
                                        # []

                                Nothing ->
                                    scanModel ! [] # []

                    Nothing ->
                        scanModel ! [] # []

            SetReceptor chainID ->
                { scanModel
                    | alanineScanSub =
                        { scanSub
                            | receptor =
                                List.append
                                    scanSub.receptor
                                    [ chainID ]
                        }
                }
                    ! [ Ports.colourGeometry ( "red", chainID ) ]
                    # []

            SetLigand chainID ->
                { scanModel
                    | alanineScanSub =
                        { scanSub
                            | ligand =
                                List.append
                                    scanSub.ligand
                                    [ chainID ]
                        }
                }
                    ! [ Ports.colourGeometry ( "blue", chainID ) ]
                    # []

            ClearReceptor chainID ->
                { scanModel
                    | alanineScanSub =
                        { scanSub
                            | receptor =
                                List.filter
                                    (\r -> r /= chainID)
                                    scanSub.receptor
                        }
                }
                    ! [ Ports.colourGeometry ( "white", chainID ) ]
                    # []

            ClearLigand chainID ->
                { scanModel
                    | alanineScanSub =
                        { scanSub
                            | ligand =
                                List.filter
                                    (\l -> l /= chainID)
                                    scanSub.ligand
                        }
                }
                    ! [ Ports.colourGeometry ( "white", chainID ) ]
                    # []

            SetScanName name ->
                { scanModel
                    | alanineScanSub =
                        { scanSub | name = name }
                }
                    ! []
                    # []

            SubmitScanJob ->
                case scanModel.structure of
                    Just structure ->
                        scanModel
                            ! [ submitAlanineScan
                                    structure
                                    scanModel.alanineScanSub
                              ]
                            # []

                    Nothing ->
                        scanModel ! [] # []

            ScanJobSubmitted (Ok jobDetails) ->
                { scanModel
                    | alanineScanSub = Model.emptyScanSub
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
                            |> List.map (\job -> ( job.jobID, job ))
                            |> Dict.fromList
                            |> Dict.insert jobDetails.jobID jobDetails
                            |> Dict.toList
                            |> List.map Tuple.second
                }
                    ! []
                    # case jobDetails.status of
                        Model.Completed ->
                            [ Notification
                                ""
                                "Alanine Scan Completed"
                                ("Alanine scan job "
                                    ++ jobDetails.name
                                    ++ " ("
                                    ++ jobDetails.jobID
                                    ++ ") complete. Retrieve the results from"
                                    ++ " the 'Jobs' tab."
                                )
                            ]

                        Model.Failed ->
                            [ Notification
                                ""
                                "Alanine Scan Failed"
                                ("Alanine scan job "
                                    ++ jobDetails.name
                                    ++ " ("
                                    ++ jobDetails.jobID
                                    ++ ") failed:\n"
                                    ++ Maybe.withDefault
                                        "Unknown error."
                                        jobDetails.stdOut
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
                    ! [ Ports.displayScanResults results ]
                    # []

            ProcessScanResults (Err error) ->
                let
                    err =
                        Debug.log "Failed to retrieve scan results" error
                in
                    scanModel ! [] # []

            DeleteScanJob jobID ->
                { scanModel
                    | jobs =
                        scanModel.jobs
                            |> List.map (\job -> ( job.jobID, job ))
                            |> Dict.fromList
                            |> Dict.remove jobID
                            |> Dict.toList
                            |> List.map Tuple.second
                }
                    ! []
                    # []

            FocusOnResidue residueResult ->
                scanModel ! [ Ports.focusOnResidue residueResult ] # []

            ClearScanSubmission ->
                { scanModel | results = Nothing, structure = Nothing } ! [] # []



-- Constellation


{-| Type that signals to `updateConstellation` how the current
`ConstellationModel` should be updated.
-}
type ConstellationMsg
    = ChangeMode String
    | UpdateAutoSettings AutoSettingsMsg
    | UpdateManualSettings ManualSettingsMsg
    | SubmitConstellationJob Model.AlanineScanResults
    | AutoJobSubmitted (Result Http.Error Model.JobDetails)
    | CheckAutoJobs Time.Time
    | ProcessAutoJobStatus (Result Http.Error Model.JobDetails)
    | GetAutoResults String
    | DeleteAutoJob String
    | ProcessAutoResults (Result Http.Error Model.ConstellationResults)
    | ManualJobSubmitted (Result Http.Error Model.JobDetails)
    | CheckManualJobs Time.Time
    | ProcessManualJobStatus (Result Http.Error Model.JobDetails)
    | GetManualResults String
    | DeleteManualJob String
    | ProcessManualResults (Result Http.Error Model.ConstellationResults)


{-| Function for updating the state of the currently active
`ConstellationModel`.
-}
updateConstellation :
    ConstellationMsg
    -> Model.ConstellationModel
    -> ( Model.ConstellationModel, Cmd ConstellationMsg, List Notification )
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
                            | constellationSub =
                                Model.Auto
                                    Model.defaultAutoSettings
                        }
                            ! []
                            # []

                    "Manual" ->
                        { model
                            | constellationSub =
                                Model.Manual
                                    Model.defaultManualSettings
                        }
                            ! []
                            # []

                    _ ->
                        model ! [] # []

            UpdateAutoSettings settingsMsg ->
                case model.constellationSub of
                    Model.Auto settings ->
                        { model
                            | constellationSub =
                                updateAutoSettings settingsMsg settings
                                    |> Model.Auto
                        }
                            ! []
                            # []

                    _ ->
                        model ! [] # []

            UpdateManualSettings settingsMsg ->
                case model.constellationSub of
                    Model.Manual settings ->
                        { model
                            | constellationSub =
                                updateManualSettings settingsMsg settings
                                    |> Model.Manual
                        }
                            ! []
                            # []

                    _ ->
                        model ! [] # []

            SubmitConstellationJob alaScanResults ->
                case model.constellationSub of
                    Model.Auto settings ->
                        model ! [ submitAutoJob alaScanResults settings ] # []

                    Model.Manual settings ->
                        model ! [ submitManualJob alaScanResults settings ] # []

            AutoJobSubmitted (Ok jobDetails) ->
                { model
                    | constellationSub = Model.Auto Model.defaultAutoSettings
                    , autoJobs = jobDetails :: model.autoJobs
                }
                    ! []
                    # []

            AutoJobSubmitted (Err error) ->
                let
                    err =
                        Debug.log "Constellation job submission error" error
                in
                    model
                        ! []
                        # [ Notifications.errorToNotification
                                "Failed to Constellation Scan Job"
                                error
                          ]

            CheckAutoJobs _ ->
                model
                    ! List.map checkAutoJobStatus
                        model.autoJobs
                    # []

            ProcessAutoJobStatus (Ok jobDetails) ->
                { model
                    | autoJobs =
                        model.autoJobs
                            |> List.map (\job -> ( job.jobID, job ))
                            |> Dict.fromList
                            |> Dict.insert jobDetails.jobID jobDetails
                            |> Dict.toList
                            |> List.map Tuple.second
                }
                    ! []
                    # case jobDetails.status of
                        Model.Completed ->
                            [ Notification
                                ""
                                "Auto Constellation Scan Completed"
                                ("Auto constellation scan job "
                                    ++ jobDetails.name
                                    ++ " ("
                                    ++ jobDetails.jobID
                                    ++ " complete. Retrieve the results from"
                                    ++ " the 'Jobs' tab."
                                )
                            ]

                        Model.Failed ->
                            [ Notification
                                ""
                                "Auto Constellation Scan Failed"
                                ("Alanine constellation scan job "
                                    ++ jobDetails.name
                                    ++ " ("
                                    ++ jobDetails.jobID
                                    ++ ") failed:\n"
                                    ++ Maybe.withDefault
                                        "Unknown error."
                                        jobDetails.stdOut
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

            DeleteAutoJob jobID ->
                { model
                    | autoJobs =
                        model.autoJobs
                            |> List.map (\job -> ( job.jobID, job ))
                            |> Dict.fromList
                            |> Dict.remove jobID
                            |> Dict.toList
                            |> List.map Tuple.second
                }
                    ! []
                    # []

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

            ManualJobSubmitted (Ok jobDetails) ->
                { model
                    | constellationSub = Model.Manual Model.defaultManualSettings
                    , manualJobs = jobDetails :: model.manualJobs
                }
                    ! []
                    # []

            ManualJobSubmitted (Err error) ->
                let
                    err =
                        Debug.log
                            "Manual Constellation job submission error"
                            error
                in
                    model
                        ! []
                        # [ Notifications.errorToNotification
                                "Failed to submit manual constellation scan job"
                                error
                          ]

            CheckManualJobs _ ->
                model
                    ! List.map checkManualJobStatus
                        model.manualJobs
                    # []

            ProcessManualJobStatus (Ok jobDetails) ->
                { model
                    | manualJobs =
                        model.manualJobs
                            |> List.map (\job -> ( job.jobID, job ))
                            |> Dict.fromList
                            |> Dict.insert jobDetails.jobID jobDetails
                            |> Dict.toList
                            |> List.map Tuple.second
                }
                    ! []
                    # case jobDetails.status of
                        Model.Completed ->
                            [ Notification
                                ""
                                "Manual Constellation Scan Completed"
                                ("Manual constellation scan job "
                                    ++ jobDetails.name
                                    ++ " ("
                                    ++ jobDetails.jobID
                                    ++ " complete. Retrieve the results from"
                                    ++ " the 'Jobs' tab."
                                )
                            ]

                        Model.Failed ->
                            [ Notification
                                ""
                                "Manual Constellation Scan Failed"
                                ("Alanine constellation scan job "
                                    ++ jobDetails.name
                                    ++ " ("
                                    ++ jobDetails.jobID
                                    ++ ") failed:\n"
                                    ++ Maybe.withDefault
                                        "Unknown error."
                                        jobDetails.stdOut
                                )
                            ]

                        _ ->
                            []

            ProcessManualJobStatus (Err error) ->
                let
                    err =
                        Debug.log "Manual job status check error" error
                in
                    model ! [] # []

            GetManualResults jobID ->
                model ! [ getManualResults jobID ] # []

            DeleteManualJob jobID ->
                { model
                    | manualJobs =
                        model.manualJobs
                            |> List.map (\job -> ( job.jobID, job ))
                            |> Dict.fromList
                            |> Dict.remove jobID
                            |> Dict.toList
                            |> List.map Tuple.second
                }
                    ! []
                    # []

            ProcessManualResults (Ok results) ->
                { model | results = Just results }
                    ! []
                    # []

            ProcessManualResults (Err error) ->
                let
                    err =
                        Debug.log "Failed to retrieve scan results" error
                in
                    model ! [] # []


type AutoSettingsMsg
    = UpdateAutoName String
    | UpdateDDGCutOff String
    | UpdateSize String
    | UpdateDistanceCutOff String


{-| Small helper update that handles user input for creating AutoSettings`
-}
updateAutoSettings : AutoSettingsMsg -> Model.AutoSettings -> Model.AutoSettings
updateAutoSettings msg settings =
    case msg of
        UpdateAutoName name ->
            { settings | name = name }

        UpdateDDGCutOff ddGValue ->
            { settings | ddGCutOff = ddGValue }

        UpdateSize constellationSize ->
            { settings | constellationSize = constellationSize }

        UpdateDistanceCutOff distance ->
            { settings | cutOffDistance = distance }


type ManualSettingsMsg
    = UpdateManualName String
    | SelectResidue Model.ResidueResult


{-| Small helper update that handles user input for creating AutoSettings`
-}
updateManualSettings : ManualSettingsMsg -> Model.ManualSettings -> Model.ManualSettings
updateManualSettings msg settings =
    case msg of
        UpdateManualName name ->
            { settings | name = name }

        SelectResidue residueResult ->
            let
                residueID =
                    residueResult.chainID ++ residueResult.residueNumber
            in
                case Set.member residueID settings.residues of
                    True ->
                        { settings
                            | residues =
                                Set.remove residueID settings.residues
                        }

                    False ->
                        { settings
                            | residues =
                                Set.insert residueID settings.residues
                        }



{- These functions create commands that create HTTP requests. -}


{-| Submits an alanine scan job to the server.
-}
submitAlanineScan : Model.Structure -> Model.AlanineScanSub -> Cmd ScanMsg
submitAlanineScan structure scanSub =
    Http.send ScanJobSubmitted <|
        Http.post
            "/api/v0.1/alanine-scan-jobs"
            (Model.encodeAlanineScanSub structure scanSub
                |> Http.jsonBody
            )
            Model.jobDetailsDecoder


{-| Checks the status of an alanine scan job on the server.
-}
checkScanJobStatus : Model.JobDetails -> Cmd ScanMsg
checkScanJobStatus { jobID } =
    Http.send ProcessScanStatus <|
        Http.get
            ("/api/v0.1/alanine-scan-job/" ++ jobID ++ "?get-status")
            Model.jobDetailsDecoder


{-| Gets the results of an alanine scan job from the server.
-}
getScanResults : String -> Cmd ScanMsg
getScanResults jobID =
    Http.send ProcessScanResults <|
        Http.get
            ("/api/v0.1/alanine-scan-job/" ++ jobID ++ "?get-results")
            Model.scanResultsDecoder


{-| Submits an auto job to the server. Receives a string back containing
the job id if sucessful.
-}
submitAutoJob :
    Model.AlanineScanResults
    -> Model.AutoSettings
    -> Cmd ConstellationMsg
submitAutoJob scanResults settings =
    Http.send AutoJobSubmitted <|
        Http.post
            "/api/v0.1/auto-jobs"
            (Model.encodeAutoJob scanResults settings
                |> Http.jsonBody
            )
            Model.jobDetailsDecoder


{-| Submits a manual job to the server. Receives a string back containing
the job id if sucessful.
-}
submitManualJob :
    Model.AlanineScanResults
    -> Model.ManualSettings
    -> Cmd ConstellationMsg
submitManualJob scanResults settings =
    Http.send ManualJobSubmitted <|
        Http.post
            "/api/v0.1/manual-jobs"
            (Model.encodeManualJob scanResults settings
                |> Http.jsonBody
            )
            Model.jobDetailsDecoder


{-| Checks the status of an auto constellation job on the server.
-}
checkAutoJobStatus : Model.JobDetails -> Cmd ConstellationMsg
checkAutoJobStatus { jobID } =
    Http.send ProcessAutoJobStatus <|
        Http.get
            ("/api/v0.1/auto-job/" ++ jobID ++ "?get-status")
            Model.jobDetailsDecoder


{-| Gets the results of an auto constellation job from the server.
-}
getAutoResults : String -> Cmd ConstellationMsg
getAutoResults jobID =
    Http.send ProcessAutoResults <|
        Http.get
            ("/api/v0.1/auto-job/" ++ jobID ++ "?get-results")
            Model.autoResultsDecoder


{-| Checks the status of a manual constellation job on the server.
-}
checkManualJobStatus : Model.JobDetails -> Cmd ConstellationMsg
checkManualJobStatus { jobID } =
    Http.send ProcessManualJobStatus <|
        Http.get
            ("/api/v0.1/manual-job/" ++ jobID ++ "?get-status")
            Model.jobDetailsDecoder


{-| Gets the results of an auto constellation job from the server.
-}
getManualResults : String -> Cmd ConstellationMsg
getManualResults jobID =
    Http.send ProcessManualResults <|
        Http.get
            ("/api/v0.1/manual-job/" ++ jobID ++ "?get-results")
            Model.autoResultsDecoder



-- HELPER FUNCTIONS


{-| Removes an item for a list at a particular position if list is long enough,
otherwise leaves the list unchanged.
-}
removeListItem : Int -> List a -> List a
removeListItem idx editList =
    List.indexedMap (,) editList
        |> List.filter (\( refIdx, _ ) -> refIdx /= idx)
        |> List.map Tuple.second
