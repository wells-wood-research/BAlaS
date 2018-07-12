port module Main exposing (..)

{-| This module provides the main entry point to the BALS web application
front end.

This file is composed of 4 sections:

1.  **MODEL** - Contains the types that describe the state of the application,
    as well functions creating, validating and encode/decoding them to/from
    JSON.
2.  **UPDATE** - The update functions and message types used to update the state
    of the model based on user input.
3.  **COMMANDS AND SUBSCRIPTIONS** - Handles communication with the world
    outside the Elm application, including JavaScript interopt and HTTP
    requests.
4.  **VIEWS** - All the functions that describe how the current state of the
    application could be rendered in the browser.

This file is currently quite long, which is not uncommon in Elm applications
(see [this](https://youtu.be/XpDsk374LDE) video for more info about why that
is), but it could be broken down later on if it gets really unwieldy.

-}

import Dict
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Model
import Notifications exposing ((#), Notification)
import Set
import Time


main : Program (Maybe Model.ExportableModel) Model.Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


{-| Creates a model and Cmd Msgs for the initial state of the application.
-}
init : Maybe Model.ExportableModel -> ( Model.Model, Cmd msg )
init mExported =
    case mExported of
        Just exported ->
            Model.importModel exported ! [ initialiseViewer () ]

        Nothing ->
            Model.emptyModel ! [ initialiseViewer () ]



{- # UPDATE
   This section contains all code that updates the state of the application.
-}


{-| Type that signals to the main update function how the model should be
updated.
-}
type Msg
    = SetAppMode Model.AppMode
    | UpdateScan ScanMsg
    | UpdateConstellation ConstellationMsg
    | DismissNotification Int
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

        DismissNotification idx ->
            { model
                | notifications =
                    removeListItem idx
                        model.notifications
            }
                ! []

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
            model ! [ Model.exportModel model |> saveState ]


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
    | UpdateStructure Model.Structure
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
                scanModel ! [ requestPDBFile () ] # []

            UpdateStructure structure ->
                { scanModel | structure = Just structure } ! [] # []

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
                                        ! [ setVisibility ( label, hidden ) ]
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
                    ! [ colourGeometry ( "red", chainID ) ]
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
                    ! [ colourGeometry ( "blue", chainID ) ]
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
                    ! [ colourGeometry ( "white", chainID ) ]
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
                    ! [ colourGeometry ( "white", chainID ) ]
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
                    ! [ displayScanResults results ]
                    # []

            ProcessScanResults (Err error) ->
                let
                    err =
                        Debug.log "Failed to retrieve scan results" error
                in
                    scanModel ! [] # []

            FocusOnResidue residueResult ->
                scanModel ! [ focusOnResidue residueResult ] # []

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
    | ProcessAutoResults (Result Http.Error Model.ConstellationResults)
    | ManualJobSubmitted (Result Http.Error Model.JobDetails)
    | CheckManualJobs Time.Time
    | ProcessManualJobStatus (Result Http.Error Model.JobDetails)
    | GetManualResults String
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



{- # COMMANDS AND SUBSCRIPTIONS
   This section contains all code for communicating with the outside world, whether
   that's with the server or javascript inside the index.html file.
-}
{- Each of the following ports corresponds to a JavaScript function in
   index.html. The function is ran when the port command is returned by the update
   function.
-}


port saveState : Model.ExportableModel -> Cmd msg


port initialiseViewer : () -> Cmd msg


port requestPDBFile : () -> Cmd msg


port setVisibility : ( String, Bool ) -> Cmd msg


port colourGeometry : ( String, Model.ChainID ) -> Cmd msg


port displayScanResults : Model.AlanineScanResults -> Cmd msg


port focusOnResidue : Model.ResidueResult -> Cmd msg



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



-- Subscriptions
{- The following ports are triggered by JavaScript functions in index.html and
   send data into the Elm application.
-}


port receiveStructure : (Model.ExportableStructure -> msg) -> Sub msg


port atomClick : (Model.ResidueInfo -> msg) -> Sub msg


{-| Describes the active subscriptions based on the current state of the model.
-}
subscriptions : Model.Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ receiveStructure <| UpdateScan << UpdateStructure

        -- , atomClick <| UpdateScan << ToggleResidueSelection
        ]
            ++ (if
                    List.length
                        (Model.getActiveJobs model.alanineScan.jobs)
                        > 0
                then
                    [ Time.every (5 * Time.second)
                        (UpdateScan << CheckScanJobs)
                    ]
                else
                    []
               )
            ++ (if
                    List.length
                        (Model.getActiveJobs model.constellation.autoJobs)
                        > 0
                then
                    [ Time.every (5 * Time.second)
                        (UpdateConstellation << CheckAutoJobs)
                    ]
                else
                    []
               )
            ++ (if
                    List.length
                        (Model.getActiveJobs model.constellation.manualJobs)
                        > 0
                then
                    [ Time.every (5 * Time.second)
                        (UpdateConstellation << CheckManualJobs)
                    ]
                else
                    []
               )



{- # VIEWS
   This section contains all functions for displaying the current state of the
   model.
-}


{-| The main view function that is called every time the model is updated.
-}
view : Model.Model -> Html Msg
view model =
    div [ class "main-grid" ]
        ([ div [ class "banner" ]
            [ header [] [ h1 [] [ text "BUDE Alanine Scan" ] ]
            , div [ class "controls" ]
                -- [ div
                --     [ class "control-button"
                --     , onClick <| OpenPanel ViewerOptions
                --     ]
                --     [ text "⚛️" ]
                [ div
                    [ class "control-button"
                    , onClick <| OpenPanel Model.Notifications
                    ]
                    [ List.length model.notifications
                        |> toString
                        |> (++) "🔔"
                        |> text
                    ]
                ]
            ]
         , div [ id "viewer" ] []
         , div [ class "tabs" ]
            [ div
                [ class "tab scan-tab"
                , onClick <| SetAppMode Model.Scan
                ]
                [ text "Scan" ]
            , div
                [ class "tab constellation-tab"
                , onClick <|
                    SetAppMode Model.Constellation
                ]
                [ text "Constellation" ]
            , div
                [ class "tab jobs-tab"
                , onClick <| SetAppMode Model.Jobs
                ]
                [ text "Jobs" ]
            ]
         , (case model.appMode of
                Model.Scan ->
                    case model.alanineScan.results of
                        Just results ->
                            scanResultsView UpdateScan results

                        Nothing ->
                            scanSubmissionView
                                UpdateScan
                                model.alanineScan.structure
                                model.alanineScan.alanineScanSub

                Model.Constellation ->
                    case model.constellation.results of
                        Just results ->
                            constellationResultsView results

                        Nothing ->
                            constellationSubmissionView
                                UpdateConstellation
                                model.constellation
                                model.alanineScan.results

                Model.Jobs ->
                    jobsView model
           )
         ]
            ++ case model.openPanel of
                Model.Notifications ->
                    [ notificationPanel model.notifications ]

                Model.ViewerOptions ->
                    [ viewerOptions model.alanineScan.structure ]

                Model.Closed ->
                    []
        )


{-| Side panel that displays notifications. Can be toggled on and off.
-}
notificationPanel : List Notification -> Html Msg
notificationPanel notifications =
    div [ class "notification-panel" ]
        [ h3 [] [ text "Notifications" ]
        , button [ onClick ClosePanel ] [ text "Close" ]
        , div [] <| List.indexedMap notificationView notifications
        ]


{-| Displays an individual notification.
-}
notificationView : Int -> Notification -> Html Msg
notificationView idx notification =
    div [ class "notification" ]
        [ h4 [] [ text notification.title ]
        , button [ onClick <| DismissNotification idx ] [ text "Dismiss" ]
        , p [ class "details" ]
            [ text notification.message ]
        ]


{-| Side panel that displays viewer options.
-}
viewerOptions : Maybe Model.Structure -> Html Msg
viewerOptions mStructure =
    div [ class "notification-panel" ]
        [ h3 [] [ text "Viewer Options" ]
        , button [ onClick ClosePanel ] [ text "Close" ]
        , case mStructure of
            Just structure ->
                div []
                    [ table []
                        ([ tr []
                            [ th [] [ text "Selection" ]
                            , th [] [ text "Hidden" ]
                            ]
                         ]
                            ++ (List.map2 (,)
                                    structure.chainLabels
                                    structure.hidden
                                    |> List.map viewerSelectionRow
                               )
                        )
                    ]

            Nothing ->
                div []
                    [ text "Load a structure before changing structure." ]
        ]


viewerSelectionRow : ( Model.ChainID, Bool ) -> Html Msg
viewerSelectionRow ( chainID, hidden ) =
    tr []
        [ td [] [ text chainID ]
        , td []
            [ input
                [ onClick <| UpdateScan <| ChangeVisibility chainID
                , type_ "checkbox"
                , checked hidden
                ]
                []
            ]
        , td [] []
        , td [] []
        ]



-- Scan


{-| Main view for the scan tab when in submission mode. This is hidden in
results mode.
-}
scanSubmissionView :
    (ScanMsg -> msg)
    -> Maybe Model.Structure
    -> Model.AlanineScanSub
    -> Html msg
scanSubmissionView updateMsg mStructure scanSub =
    div
        [ class "control-panel scan-panel" ]
        [ h2 [] [ text "Scan Submission" ]
        , div []
            [ input [ type_ "file", id "pdbFileToLoad" ] []
            , button [ onClick <| updateMsg GetStructure ] [ text "Upload" ]
            ]
        , div []
            (case mStructure of
                Just structure ->
                    [ div []
                        [ text "Job Name"
                        , br [] []
                        , input [ onInput <| updateMsg << SetScanName ] []
                        ]
                    , table []
                        ([ tr []
                            [ th [] [ text "Chain" ]
                            , th [] [ text "Receptor" ]
                            , th [] [ text "Ligand" ]
                            ]
                         ]
                            ++ (List.map
                                    (chainSelect updateMsg
                                        scanSub.receptor
                                        scanSub.ligand
                                    )
                                    structure.chainLabels
                               )
                        )
                    , button
                        [ onClick <| updateMsg SubmitScanJob
                        , Model.validScanSub scanSub |> not |> disabled
                        ]
                        [ text "Start Scan" ]
                    ]

                Nothing ->
                    []
            )
        ]


{-| Main view for the scan tab when in results mode. This is hidden in
submission mode.
-}
scanResultsView : (ScanMsg -> msg) -> Model.AlanineScanResults -> Html msg
scanResultsView updateMsg results =
    div
        [ class "control-panel scan-panel" ]
        [ h2 [] [ text "Alanine Scan Results" ]
        , button
            [ onClick <| updateMsg ClearScanSubmission ]
            [ text "New Submission" ]
        , h3 [] [ text "Job Name" ]
        , p [] [ text results.name ]
        , h3 [] [ text "ΔG" ]
        , p [] [ toString results.dG |> text ]
        , h3 [] [ text "Residue Results (Non-Zero)" ]
        , scanResultsTable updateMsg results.ligandResults
        ]


scanResultsTable :
    (ScanMsg -> msg)
    -> List Model.ResidueResult
    -> Html msg
scanResultsTable updateMsg ligandResults =
    let
        resultsRow resResult =
            let
                { chainID, residueNumber, aminoAcid, ddG } =
                    resResult
            in
                tr
                    [ onClick <| updateMsg <| FocusOnResidue resResult ]
                    [ td [] [ text chainID ]
                    , td [] [ text residueNumber ]
                    , td [] [ text aminoAcid ]
                    , td [] [ toString ddG |> text ]
                    ]
    in
        table [ class "scan-results-table" ]
            ([ tr []
                [ th [] [ text "Chain" ]
                , th [] [ text "Residue" ]
                , th [] [ text "Amino Acid" ]
                , th [] [ text "ΔΔG" ]
                ]
             ]
                ++ (List.filter (\res -> res.ddG /= 0) ligandResults
                        |> List.map resultsRow
                   )
            )


{-| View for selecting the receptor and ligand chains.
-}
chainSelect :
    (ScanMsg -> msg)
    -> List Model.ChainID
    -> List Model.ChainID
    -> Model.ChainID
    -> Html msg
chainSelect updateMsg receptorLabels ligandLabels label =
    tr []
        [ td [] [ text label ]
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



-- Constellations


{-| Main view for the constellation tab when in submission mode. This is hidden
in results mode.
-}
constellationSubmissionView :
    (ConstellationMsg -> msg)
    -> Model.ConstellationModel
    -> Maybe Model.AlanineScanResults
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


{-| Actual submission view for constellation jobs. Only shown if a valid
`AlanineScanResults` is active in the model.
-}
activeConstellationSub :
    (ConstellationMsg -> msg)
    -> Model.ConstellationModel
    -> Model.AlanineScanResults
    -> Html msg
activeConstellationSub updateMsg model scanRes =
    let
        modeString =
            case model.constellationSub of
                Model.Auto _ ->
                    "Auto"

                Model.Manual _ ->
                    "Manual"
    in
        div []
            [ h3 [] [ text "Select Mode" ]
            , select [ onInput <| updateMsg << ChangeMode ] <|
                List.map (simpleOption modeString)
                    [ "Auto", "Manual" ]
            , case model.constellationSub of
                Model.Auto settings ->
                    autoSettingsView updateMsg scanRes settings

                Model.Manual settings ->
                    manualSettingsView updateMsg scanRes settings
            ]


{-| View for submission of auto settings input.
-}
autoSettingsView :
    (ConstellationMsg -> msg)
    -> Model.AlanineScanResults
    -> Model.AutoSettings
    -> Html msg
autoSettingsView updateMsg scanRes settings =
    let
        { ddGCutOff, constellationSize, cutOffDistance } =
            settings
    in
        div []
            [ h3 [] [ text "Job Name" ]
            , input
                [ onInput <| updateMsg << UpdateAutoSettings << UpdateAutoName
                ]
                []
            , h3 [] [ text "ΔΔG Cut Off Value" ]
            , input
                [ onInput <| updateMsg << UpdateAutoSettings << UpdateDDGCutOff
                , pattern "[+-]?([0-9]*[.])?[0-9]+"
                , value ddGCutOff
                , placeholder "ΔΔG"
                ]
                []
            , h3 [] [ text "Constellation Size" ]
            , input
                [ onInput <| updateMsg << UpdateAutoSettings << UpdateSize
                , pattern "[0-9]*"
                , value constellationSize
                , placeholder "Size"
                ]
                []
            , h3 [] [ text "Cut Off Distance" ]
            , input
                [ onInput <| updateMsg << UpdateAutoSettings << UpdateDistanceCutOff
                , pattern "[+-]?([0-9]*[.])?[0-9]+"
                , value cutOffDistance
                , placeholder "Distance"
                ]
                []
            , br [] []
            , button
                [ onClick <| updateMsg <| SubmitConstellationJob scanRes
                , disabled <| not <| Model.validAutoSettings settings
                ]
                [ text "Submit" ]
            ]


{-| View for submission of manual settings input.
-}
manualSettingsView :
    (ConstellationMsg -> msg)
    -> Model.AlanineScanResults
    -> Model.ManualSettings
    -> Html msg
manualSettingsView updateMsg scanResults settings =
    let
        { residues } =
            settings
    in
        div []
            [ h3 [] [ text "Job Name" ]
            , input
                [ onInput <|
                    updateMsg
                        << UpdateManualSettings
                        << UpdateManualName
                ]
                []
            , h3 [] [ text "Select Residues" ]
            , text "Click to select."
            , manualSelectTable
                updateMsg
                residues
                scanResults.ligandResults
            , br [] []
            , button
                [ onClick <| updateMsg <| SubmitConstellationJob scanResults
                , disabled <| not <| Model.validManualSettings settings
                ]
                [ text "Submit" ]
            ]


manualSelectTable :
    (ConstellationMsg -> msg)
    -> Set.Set String
    -> List Model.ResidueResult
    -> Html msg
manualSelectTable updateMsg selected ligandResults =
    let
        resultsRow resResult =
            let
                { chainID, residueNumber, aminoAcid, ddG } =
                    resResult

                residueID =
                    chainID ++ residueNumber
            in
                tr
                    [ case Set.member residueID selected of
                        True ->
                            class "selected-residue"

                        False ->
                            class "unselected-residue"
                    , onClick <|
                        updateMsg <|
                            UpdateManualSettings <|
                                SelectResidue resResult
                    ]
                    [ td [] [ text chainID ]
                    , td [] [ text residueNumber ]
                    , td [] [ text aminoAcid ]
                    , td [] [ toString ddG |> text ]
                    ]
    in
        table [ class "scan-results-table" ]
            ([ tr []
                [ th [] [ text "Chain" ]
                , th [] [ text "Residue" ]
                , th [] [ text "Amino Acid" ]
                , th [] [ text "ΔΔG" ]
                ]
             ]
                ++ (List.filter (\res -> res.ddG /= 0) ligandResults
                        |> List.map resultsRow
                   )
            )


{-| Main view for the constellation tab when in results mode. This is hidden
in submission mode.
-}
constellationResultsView : Model.ConstellationResults -> Html msg
constellationResultsView { hotConstellations } =
    let
        resultsRow ( constellation, meanDDG ) =
            tr []
                [ td [] [ text constellation ]
                , td [] [ text <| toString meanDDG ]
                ]
    in
        div
            [ class "control-panel constellation-panel" ]
            [ h2 [] [ text "Constellation Scan Results" ]
            , h3 [] [ text "🔥 Hot Constellations 🔥" ]
            , table [ class "scan-results-table" ]
                ([ tr []
                    [ th [] [ text "Constellation" ]
                    , th [] [ text "Mean ΔΔG" ]
                    ]
                 ]
                    ++ List.map resultsRow hotConstellations
                )
            ]



-- Jobs


{-| Main view for the Jobs tabs.
-}
jobsView : Model.Model -> Html Msg
jobsView model =
    let
        alaScanJobs =
            model.alanineScan.jobs

        autoJobs =
            model.constellation.autoJobs

        manualJobs =
            model.constellation.manualJobs
    in
        div [ class "control-panel jobs-panel" ]
            [ h2 [] [ text "Jobs" ]
            , jobTable (UpdateScan << GetScanResults)
                "Alanine Scan Jobs"
                (List.sortBy (\{ name } -> name) alaScanJobs)
            , jobTable (UpdateConstellation << GetAutoResults)
                "Auto Constellation Scan Jobs"
                (List.sortBy (\{ name } -> name) autoJobs)
            , jobTable (UpdateConstellation << GetManualResults)
                "Manual Constellation Scan Jobs"
                (List.sortBy (\{ name } -> name) manualJobs)
            ]


{-| Creates a table of job details. Takes a `Msg` constructor as an argument so
that it can be reused for all the job queues.
-}
jobTable : (String -> Msg) -> String -> List Model.JobDetails -> Html Msg
jobTable getMsg tableTitle jobs =
    let
        tableRow { jobID, name, status } =
            tr [ class "details" ]
                [ td [] [ text name ]
                , td [] [ text jobID ]
                , td [] [ text <| toString status ]
                , td []
                    [ button
                        [ getMsg jobID
                            |> onClick
                        , (if (status == Model.Completed) then
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
                table [ class "jobs-table" ]
                    ([ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Job ID" ]
                        , th [] [ text "Status" ]
                        , th [] []
                        ]
                     ]
                        ++ List.map tableRow jobs
                    )
              else
                text "No Jobs submitted."
            ]



-- Misc helper functions


{-| Removes an item for a list at a particular position if list is long enough,
otherwise leaves the list unchanged.
-}
removeListItem : Int -> List a -> List a
removeListItem idx editList =
    List.indexedMap (,) editList
        |> List.filter (\( refIdx, _ ) -> refIdx /= idx)
        |> List.map Tuple.second


simpleOption : String -> String -> Html msg
simpleOption selectedLabel labelValue =
    case selectedLabel == labelValue of
        True ->
            option [ value labelValue, selected True ] [ text labelValue ]

        False ->
            option [ value labelValue ] [ text labelValue ]
