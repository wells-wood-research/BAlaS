module Update exposing
    ( AutoSettingsMsg(..)
    , ConstellationMsg(..)
    , ManualSettingsMsg(..)
    , Msg(..)
    , ResiduesSettingsMsg(..)
    , ScanMsg(..)
    , update
    )

{- # UPDATE
   This section contains all code that updates the state of the application.
-}

import Dict
import Http
import Model
import Notifications exposing (Notification)
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
    | ColourResidues Model.ResidueColour
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
            ( { model | appMode = appMode }
            , Cmd.none
            )

        UpdateScan scanMsg ->
            let
                constellation =
                    model.constellation

                ( updatedScanModel, scanSubCmds, notifications ) =
                    updateScan scanMsg model.alanineScan
            in
            case scanMsg of
                ClearScanSubmission ->
                    ( { model
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
                    , Cmd.map UpdateScan scanSubCmds
                    )

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
                    ( updatedModel
                    , Cmd.batch
                        [ Cmd.map UpdateScan scanSubCmds
                        , updatedCmds
                        ]
                    )

                ProcessScanResults (Ok _) ->
                    ( { model
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
                    , Cmd.map UpdateScan scanSubCmds
                    )

                _ ->
                    ( { model
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
                    , Cmd.map UpdateScan scanSubCmds
                    )

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

                ResiduesJobSubmitted (Ok _) ->
                    sucessfulConSubmit model constellationMsg

                ProcessResiduesResults (Ok { scanResults }) ->
                    sucessfulConResults model scanResults constellationMsg

                _ ->
                    let
                        ( updatedConModel, conSubCmds, notifications ) =
                            updateConstellation constellationMsg model.constellation
                    in
                    ( { model
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
                    , Cmd.map UpdateConstellation conSubCmds
                    )

        SetHoveredName mName ->
            ( { model | hoveredName = mName }
            , Cmd.none
            )

        DismissNotification idx ->
            ( { model
                | notifications =
                    removeListItem idx
                        model.notifications
              }
            , Cmd.none
            )

        ClearNotifications ->
            ( { model | notifications = [] }
            , Cmd.none
            )

        OpenPanel panel ->
            case model.openPanel of
                Model.Closed ->
                    ( { model | openPanel = panel }
                    , Cmd.none
                    )

                _ ->
                    ( { model | openPanel = Model.Closed }
                    , Cmd.none
                    )

        ClosePanel ->
            ( { model | openPanel = Model.Closed }
            , Cmd.none
            )

        ColourResidues residueColour ->
            ( model
            , Ports.colourResidues residueColour
            )

        SaveState ->
            ( model
            , Model.exportModel model |> Ports.saveState
            )


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
    ( updatedModel
    , Cmd.batch
        [ Cmd.map UpdateConstellation conSubCmds
        , updatedCmds
        ]
    )


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
    ( { updatedModel
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
    , Cmd.batch
        [ Cmd.map UpdateConstellation conSubCmds
        , scanCmds
        ]
    )



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
    | CheckScanJobs Time.Posix
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
            ( scanModel
            , Ports.requestPDBFile ()
            , []
            )

        UpdateStructure structure ->
            ( { scanModel | structure = structure }
            , Cmd.none
            , []
            )

        ChangeVisibility label ->
            case scanModel.structure of
                Just structure ->
                    let
                        hiddenDict =
                            structure.geometryLabels
                                |> Dict.fromList
                                |> Dict.update label (Maybe.map not)
                    in
                    case Dict.get label hiddenDict of
                        Just hidden ->
                            ( { scanModel
                                | structure =
                                    Just
                                        { structure
                                            | geometryLabels =
                                                Dict.toList hiddenDict
                                        }
                              }
                            , Ports.setVisibility ( label, hidden )
                            , []
                            )

                        Nothing ->
                            ( scanModel
                            , Cmd.none
                            , []
                            )

                Nothing ->
                    ( scanModel
                    , Cmd.none
                    , []
                    )

        SetReceptor chainID ->
            ( { scanModel
                | alanineScanSub =
                    { scanSub
                        | receptor =
                            List.append
                                scanSub.receptor
                                [ chainID ]
                    }
              }
            , Ports.colourGeometry ( "red", chainID ++ "_cartoon" )
            , []
            )

        SetLigand chainID ->
            ( { scanModel
                | alanineScanSub =
                    { scanSub
                        | ligand =
                            List.append
                                scanSub.ligand
                                [ chainID ]
                    }
              }
            , Ports.colourGeometry ( "blue", chainID ++ "_cartoon" )
            , []
            )

        ClearReceptor chainID ->
            ( { scanModel
                | alanineScanSub =
                    { scanSub
                        | receptor =
                            List.filter
                                (\r -> r /= chainID)
                                scanSub.receptor
                    }
              }
            , Ports.colourGeometry ( "white", chainID ++ "_cartoon" )
            , []
            )

        ClearLigand chainID ->
            ( { scanModel
                | alanineScanSub =
                    { scanSub
                        | ligand =
                            List.filter
                                (\l -> l /= chainID)
                                scanSub.ligand
                    }
              }
            , Ports.colourGeometry ( "white", chainID ++ "_cartoon" )
            , []
            )

        SetScanName name ->
            ( { scanModel
                | alanineScanSub =
                    { scanSub | name = name }
              }
            , Cmd.none
            , []
            )

        SubmitScanJob ->
            case scanModel.structure of
                Just structure ->
                    ( scanModel
                    , submitAlanineScan
                        structure
                        scanModel.alanineScanSub
                    , []
                    )

                Nothing ->
                    ( scanModel
                    , Cmd.none
                    , []
                    )

        ScanJobSubmitted (Ok jobDetails) ->
            ( { scanModel
                | alanineScanSub = Model.emptyScanSub
                , jobs = jobDetails :: scanModel.jobs
              }
            , Cmd.none
            , []
            )

        ScanJobSubmitted (Err error) ->
            let
                err =
                    Debug.log "Scan job submission error" error
            in
            ( scanModel
            , Cmd.none
            , [ Notifications.errorToNotification
                    "Failed to Submit Scan Job"
                    error
              ]
            )

        CheckScanJobs _ ->
            ( scanModel
            , Cmd.batch
                (List.map checkScanJobStatus
                    scanModel.jobs
                )
            , []
            )

        ProcessScanStatus (Ok jobDetails) ->
            ( { scanModel
                | jobs =
                    scanModel.jobs
                        |> List.map (\job -> ( job.jobID, job ))
                        |> Dict.fromList
                        |> Dict.insert jobDetails.jobID jobDetails
                        |> Dict.toList
                        |> List.map Tuple.second
              }
            , Cmd.none
            , case jobDetails.status of
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
            )

        ProcessScanStatus (Err error) ->
            let
                err =
                    Debug.log "Scan status check error" error
            in
            ( scanModel
            , Cmd.none
            , []
            )

        GetScanResults scanID ->
            ( scanModel
            , getScanResults scanID
            , []
            )

        ProcessScanResults (Ok results) ->
            ( { scanModel | results = Just results }
            , Ports.displayScanResults results
            , []
            )

        ProcessScanResults (Err error) ->
            let
                err =
                    Debug.log "Failed to retrieve scan results" error
            in
            ( scanModel
            , Cmd.none
            , []
            )

        DeleteScanJob jobID ->
            ( { scanModel
                | jobs =
                    scanModel.jobs
                        |> List.map (\job -> ( job.jobID, job ))
                        |> Dict.fromList
                        |> Dict.remove jobID
                        |> Dict.toList
                        |> List.map Tuple.second
              }
            , Cmd.none
            , []
            )

        FocusOnResidue residueResult ->
            ( scanModel
            , Ports.focusOnResidue residueResult
            , []
            )

        ClearScanSubmission ->
            ( { scanModel | results = Nothing, structure = Nothing }
            , Cmd.none
            , []
            )



-- Constellation


{-| Type that signals to `updateConstellation` how the current
`ConstellationModel` should be updated.
-}
type ConstellationMsg
    = ChangeMode String
    | UpdateAutoSettings AutoSettingsMsg
    | UpdateManualSettings ManualSettingsMsg
    | UpdateResiduesSettings ResiduesSettingsMsg
    | SubmitConstellationJob Model.AlanineScanResults
    | AutoJobSubmitted (Result Http.Error Model.JobDetails)
    | CheckAutoJobs Time.Posix
    | ProcessAutoJobStatus (Result Http.Error Model.JobDetails)
    | GetAutoResults String
    | DeleteAutoJob String
    | ProcessAutoResults (Result Http.Error Model.ConstellationResults)
    | ManualJobSubmitted (Result Http.Error Model.JobDetails)
    | CheckManualJobs Time.Posix
    | ProcessManualJobStatus (Result Http.Error Model.JobDetails)
    | GetManualResults String
    | DeleteManualJob String
    | ProcessManualResults (Result Http.Error Model.ConstellationResults)
    | ResiduesJobSubmitted (Result Http.Error Model.JobDetails)
    | CheckResiduesJobs Time.Posix
    | ProcessResiduesJobStatus (Result Http.Error Model.JobDetails)
    | GetResiduesResults String
    | DeleteResiduesJob String
    | ProcessResiduesResults (Result Http.Error Model.ConstellationResults)


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
                    ( { model
                        | constellationSub =
                            Model.Auto
                                Model.defaultAutoSettings
                      }
                    , Cmd.none
                    , []
                    )

                "Manual" ->
                    ( { model
                        | constellationSub =
                            Model.Manual
                                Model.defaultManualSettings
                      }
                    , Cmd.none
                    , []
                    )

                "Residues" ->
                    ( { model
                        | constellationSub =
                            Model.Residues
                                Model.defaultResiduesSettings
                      }
                    , Cmd.none
                    , []
                    )

                _ ->
                    ( model
                    , Cmd.none
                    , []
                    )

        UpdateAutoSettings settingsMsg ->
            case model.constellationSub of
                Model.Auto settings ->
                    ( { model
                        | constellationSub =
                            updateAutoSettings settingsMsg settings
                                |> Model.Auto
                      }
                    , Cmd.none
                    , []
                    )

                _ ->
                    ( model
                    , Cmd.none
                    , []
                    )

        UpdateManualSettings settingsMsg ->
            case model.constellationSub of
                Model.Manual settings ->
                    let
                        ( updatedSettings, manualCmds ) =
                            updateManualSettings settingsMsg settings
                    in
                    ( { model
                        | constellationSub =
                            Model.Manual updatedSettings
                      }
                    , Cmd.map UpdateManualSettings manualCmds
                    , []
                    )

                _ ->
                    ( model
                    , Cmd.none
                    , []
                    )

        UpdateResiduesSettings settingsMsg ->
            case model.constellationSub of
                Model.Residues settings ->
                    let
                        ( updatedSettings, residuesCmds ) =
                            updateResiduesSettings settingsMsg settings
                    in
                    ( { model
                        | constellationSub =
                            Model.Residues updatedSettings
                      }
                    , Cmd.map UpdateResiduesSettings residuesCmds
                    , []
                    )

                _ ->
                    ( model
                    , Cmd.none
                    , []
                    )

        SubmitConstellationJob alaScanResults ->
            case model.constellationSub of
                Model.Auto settings ->
                    ( model
                    , submitAutoJob alaScanResults settings
                    , []
                    )

                Model.Manual settings ->
                    ( model
                    , submitManualJob alaScanResults settings
                    , []
                    )

                Model.Residues settings ->
                    ( model
                    , submitResiduesJob alaScanResults settings
                    , []
                    )

        AutoJobSubmitted (Ok jobDetails) ->
            ( { model
                | constellationSub = Model.Auto Model.defaultAutoSettings
                , autoJobs = jobDetails :: model.autoJobs
              }
            , Cmd.none
            , []
            )

        AutoJobSubmitted (Err error) ->
            let
                err =
                    Debug.log "Constellation job submission error" error
            in
            ( model
            , Cmd.none
            , [ Notifications.errorToNotification
                    "Failed to Constellation Scan Job"
                    error
              ]
            )

        CheckAutoJobs _ ->
            ( model
            , Cmd.batch
                (List.map checkAutoJobStatus
                    model.autoJobs
                )
            , []
            )

        ProcessAutoJobStatus (Ok jobDetails) ->
            ( { model
                | autoJobs =
                    model.autoJobs
                        |> List.map (\job -> ( job.jobID, job ))
                        |> Dict.fromList
                        |> Dict.insert jobDetails.jobID jobDetails
                        |> Dict.toList
                        |> List.map Tuple.second
              }
            , Cmd.none
            , case jobDetails.status of
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
            )

        ProcessAutoJobStatus (Err error) ->
            let
                err =
                    Debug.log "Auto job status check error" error
            in
            ( model
            , Cmd.none
            , []
            )

        GetAutoResults jobID ->
            ( model
            , getAutoResults jobID
            , []
            )

        DeleteAutoJob jobID ->
            ( { model
                | autoJobs =
                    model.autoJobs
                        |> List.map (\job -> ( job.jobID, job ))
                        |> Dict.fromList
                        |> Dict.remove jobID
                        |> Dict.toList
                        |> List.map Tuple.second
              }
            , Cmd.none
            , []
            )

        ProcessAutoResults (Ok results) ->
            ( { model | results = Just results }
            , Cmd.none
            , []
            )

        ProcessAutoResults (Err error) ->
            let
                err =
                    Debug.log "Failed to retrieve scan results" error
            in
            ( model
            , Cmd.none
            , []
            )

        ManualJobSubmitted (Ok jobDetails) ->
            ( { model
                | constellationSub = Model.Manual Model.defaultManualSettings
                , manualJobs = jobDetails :: model.manualJobs
              }
            , Cmd.none
            , []
            )

        ManualJobSubmitted (Err error) ->
            let
                err =
                    Debug.log
                        "Manual Constellation job submission error"
                        error
            in
            ( model
            , Cmd.none
            , [ Notifications.errorToNotification
                    "Failed to submit manual constellation scan job"
                    error
              ]
            )

        CheckManualJobs _ ->
            ( model
            , Cmd.batch
                (List.map checkManualJobStatus
                    model.manualJobs
                )
            , []
            )

        ProcessManualJobStatus (Ok jobDetails) ->
            ( { model
                | manualJobs =
                    model.manualJobs
                        |> List.map (\job -> ( job.jobID, job ))
                        |> Dict.fromList
                        |> Dict.insert jobDetails.jobID jobDetails
                        |> Dict.toList
                        |> List.map Tuple.second
              }
            , Cmd.none
            , case jobDetails.status of
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
            )

        ProcessManualJobStatus (Err error) ->
            let
                err =
                    Debug.log "Manual job status check error" error
            in
            ( model
            , Cmd.none
            , []
            )

        GetManualResults jobID ->
            ( model
            , getManualResults jobID
            , []
            )

        DeleteManualJob jobID ->
            ( { model
                | manualJobs =
                    model.manualJobs
                        |> List.map (\job -> ( job.jobID, job ))
                        |> Dict.fromList
                        |> Dict.remove jobID
                        |> Dict.toList
                        |> List.map Tuple.second
              }
            , Cmd.none
            , []
            )

        ProcessManualResults (Ok results) ->
            ( { model | results = Just results }
            , Cmd.none
            , []
            )

        ProcessManualResults (Err error) ->
            let
                err =
                    Debug.log "Failed to retrieve scan results" error
            in
            ( model
            , Cmd.none
            , []
            )

        ResiduesJobSubmitted (Ok jobDetails) ->
            ( { model
                | constellationSub = Model.Residues Model.defaultResiduesSettings
                , residuesJobs = jobDetails :: model.residuesJobs
              }
            , Cmd.none
            , []
            )

        ResiduesJobSubmitted (Err error) ->
            let
                err =
                    Debug.log
                        "Residues Constellation job submission error"
                        error
            in
            ( model
            , Cmd.none
            , [ Notifications.errorToNotification
                    "Failed to submit residues constellation scan job"
                    error
              ]
            )

        CheckResiduesJobs _ ->
            ( model
            , Cmd.batch
                (List.map checkResiduesJobStatus
                    model.residuesJobs
                )
            , []
            )

        ProcessResiduesJobStatus (Ok jobDetails) ->
            ( { model
                | residuesJobs =
                    model.residuesJobs
                        |> List.map (\job -> ( job.jobID, job ))
                        |> Dict.fromList
                        |> Dict.insert jobDetails.jobID jobDetails
                        |> Dict.toList
                        |> List.map Tuple.second
              }
            , Cmd.none
            , case jobDetails.status of
                Model.Completed ->
                    [ Notification
                        ""
                        "Residues Constellation Scan Completed"
                        ("Residues constellation scan job "
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
                        "Residues Constellation Scan Failed"
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
            )

        ProcessResiduesJobStatus (Err error) ->
            let
                err =
                    Debug.log "Residues job status check error" error
            in
            ( model
            , Cmd.none
            , []
            )

        GetResiduesResults jobID ->
            ( model
            , getResiduesResults jobID
            , []
            )

        DeleteResiduesJob jobID ->
            ( { model
                | residuesJobs =
                    model.residuesJobs
                        |> List.map (\job -> ( job.jobID, job ))
                        |> Dict.fromList
                        |> Dict.remove jobID
                        |> Dict.toList
                        |> List.map Tuple.second
              }
            , Cmd.none
            , []
            )

        ProcessResiduesResults (Ok results) ->
            ( { model | results = Just results }
            , Cmd.none
            , []
            )

        ProcessResiduesResults (Err error) ->
            let
                err =
                    Debug.log "Failed to retrieve scan results" error
            in
            ( model
            , Cmd.none
            , []
            )


type AutoSettingsMsg
    = UpdateAutoName String
    | UpdateDDGCutOff String
    | UpdateSize String
    | UpdateDistanceCutOff String


{-| Small helper update that handles user input for creating AutoSettings\`
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
    | SelectManualResidue Model.ResidueResult


{-| Small helper update that handles user input for creating ManualSettings\`
-}
updateManualSettings :
    ManualSettingsMsg
    -> Model.ManualSettings
    -> ( Model.ManualSettings, Cmd ManualSettingsMsg )
updateManualSettings msg settings =
    case msg of
        UpdateManualName name ->
            ( { settings | name = name }
            , Cmd.none
            )

        SelectManualResidue residueResult ->
            let
                residueID =
                    residueResult.chainID ++ residueResult.residueNumber
            in
            case Set.member residueID settings.residues of
                True ->
                    ( { settings
                        | residues =
                            Set.remove residueID settings.residues
                      }
                    , Model.ResidueColour
                        "ligand_ballsAndSticks"
                        [ ( residueResult.chainID
                          , residueResult.residueNumber
                          )
                        ]
                        "cpk"
                        |> Ports.colourResidues
                    )

                False ->
                    ( { settings
                        | residues =
                            Set.insert residueID settings.residues
                      }
                    , Model.ResidueColour
                        "ligand_ballsAndSticks"
                        [ ( residueResult.chainID
                          , residueResult.residueNumber
                          )
                        ]
                        "red"
                        |> Ports.colourResidues
                    )


type ResiduesSettingsMsg
    = UpdateResiduesName String
    | UpdateConstellationSize String
    | SelectResiduesResidue Model.ResidueResult


{-| Small helper update that handles user input for creating AutoSettings\`
-}
updateResiduesSettings :
    ResiduesSettingsMsg
    -> Model.ResiduesSettings
    -> ( Model.ResiduesSettings, Cmd ResiduesSettingsMsg )
updateResiduesSettings msg settings =
    case msg of
        UpdateResiduesName name ->
            ( { settings | name = name }
            , Cmd.none
            )

        UpdateConstellationSize size ->
            ( { settings
                | constellationSize =
                    String.toInt size
                        |> Maybe.withDefault 3
              }
            , Cmd.none
            )

        SelectResiduesResidue residueResult ->
            let
                residueID =
                    residueResult.chainID ++ residueResult.residueNumber
            in
            case Set.member residueID settings.residues of
                True ->
                    ( { settings
                        | residues =
                            Set.remove residueID settings.residues
                      }
                    , Model.ResidueColour
                        "ligand_ballsAndSticks"
                        [ ( residueResult.chainID
                          , residueResult.residueNumber
                          )
                        ]
                        "cpk"
                        |> Ports.colourResidues
                    )

                False ->
                    ( { settings
                        | residues =
                            Set.insert residueID settings.residues
                      }
                    , Model.ResidueColour
                        "ligand_ballsAndSticks"
                        [ ( residueResult.chainID
                          , residueResult.residueNumber
                          )
                        ]
                        "red"
                        |> Ports.colourResidues
                    )



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
the job id if successful.
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
the job id if successful.
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


{-| Submits a residues job to the server. Receives a string back containing
the job id if successful.
-}
submitResiduesJob :
    Model.AlanineScanResults
    -> Model.ResiduesSettings
    -> Cmd ConstellationMsg
submitResiduesJob scanResults settings =
    Http.send ResiduesJobSubmitted <|
        Http.post
            "/api/v0.1/residues-jobs"
            (Model.encodeResiduesJob scanResults settings
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


{-| Gets the results of an manual constellation job from the server.
-}
getManualResults : String -> Cmd ConstellationMsg
getManualResults jobID =
    Http.send ProcessManualResults <|
        Http.get
            ("/api/v0.1/manual-job/" ++ jobID ++ "?get-results")
            Model.autoResultsDecoder


{-| Checks the status of a residues constellation job on the server.
-}
checkResiduesJobStatus : Model.JobDetails -> Cmd ConstellationMsg
checkResiduesJobStatus { jobID } =
    Http.send ProcessResiduesJobStatus <|
        Http.get
            ("/api/v0.1/residues-job/" ++ jobID ++ "?get-status")
            Model.jobDetailsDecoder


{-| Gets the results of an residues constellation job from the server.
-}
getResiduesResults : String -> Cmd ConstellationMsg
getResiduesResults jobID =
    Http.send ProcessResiduesResults <|
        Http.get
            ("/api/v0.1/residues-job/" ++ jobID ++ "?get-results")
            Model.autoResultsDecoder



-- HELPER FUNCTIONS


{-| Removes an item for a list at a particular position if list is long enough,
otherwise leaves the list unchanged.
-}
removeListItem : Int -> List a -> List a
removeListItem idx editList =
    List.indexedMap (\a b -> ( a, b )) editList
        |> List.filter (\( refIdx, _ ) -> refIdx /= idx)
        |> List.map Tuple.second
