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
import Json.Decode as JDe
import Json.Decode.Extra exposing ((|:))
import Json.Encode as JEn
import Notifications exposing ((#), Notification)
import Time


main : Program (Maybe ExportableModel) Model Msg
main =
    programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



{- # MODEL
   This section contains the types that describe the state of the application,
   as well functions for creating, validating and encode/decoding them to/from
   JSON.
-}


{-| Describes the current state of the application
-}
type alias Model =
    { appMode : AppMode
    , alanineScan : AlanineScanModel
    , constellation : ConstellationModel
    , notifications : List Notification
    , openPanel : Panel
    }


type Panel
    = Notifications
    | ViewerOptions
    | Closed


emptyModel : Model
emptyModel =
    Model
        Scan
        emptyAlaScanModel
        emptyConstellationModel
        []
        Closed


type alias ExportableModel =
    { alanineScan : List ExportableJobDetails
    , constellation : List ExportableJobDetails
    , notifications : List Notification
    }


exportModel : Model -> ExportableModel
exportModel model =
    ExportableModel
        (List.map exportJobDetails model.alanineScan.jobs)
        (List.map exportJobDetails model.constellation.jobs)
        model.notifications


importModel : ExportableModel -> Model
importModel exported =
    Model
        Scan
        { emptyAlaScanModel
            | jobs =
                List.map importJobDetails exported.alanineScan
        }
        { emptyConstellationModel
            | jobs =
                List.map importJobDetails exported.constellation
        }
        exported.notifications
        Closed


{-| Modes available in the app, corresponding to the 3 main tabs.
-}
type AppMode
    = Scan
    | Constellation
    | Jobs


{-| Creates a model and Cmd Msgs for the initial state of the application.
-}
init : Maybe ExportableModel -> ( Model, Cmd msg )
init mExported =
    case mExported of
        Just exported ->
            importModel exported ! [ initialiseViewer () ]

        Nothing ->
            emptyModel ! [ initialiseViewer () ]



-- Utility types


{-| Represents a chain label in a PDB file.
-}
type alias ChainID =
    String


{-| A helpful record describing the details of a single residue.
-}
type alias ResidueInfo =
    { chainID : ChainID
    , aminoAcid : String
    , residueNumber : Int
    }



-- Scan Mode


{-| Submodel that represents the state for the scan tab.
-}
type alias AlanineScanModel =
    { alanineScanSub : AlanineScanSub
    , structure : Maybe Structure
    , jobs : List JobDetails
    , results : Maybe AlanineScanResults
    }


emptyAlaScanModel : AlanineScanModel
emptyAlaScanModel =
    AlanineScanModel emptyScanSub Nothing [] Nothing


{-| Form data required for a scan job submission.
-}
type alias AlanineScanSub =
    { name : String
    , receptor : List ChainID
    , ligand : List ChainID
    }


emptyScanSub : AlanineScanSub
emptyScanSub =
    AlanineScanSub
        ""
        []
        []


{-| Validates an `AlanineScanSub` to determine if it can be safely submitted.
-}
validScanSub : AlanineScanSub -> Bool
validScanSub { name, receptor, ligand } =
    let
        isJust mA =
            case mA of
                Just _ ->
                    True

                Nothing ->
                    False
    in
        (name /= "")
            && (List.length receptor > 0)
            && (List.length ligand > 0)


{-| JSON encoder for `AlanineScanSub`
-}
encodeAlanineScanSub : Structure -> AlanineScanSub -> JEn.Value
encodeAlanineScanSub structure scanSub =
    JEn.object
        [ ( "name", scanSub.name |> JEn.string )
        , ( "pdbFile", JEn.string structure.pdbFile )
        , ( "receptor", List.map JEn.string scanSub.receptor |> JEn.list )
        , ( "ligand", List.map JEn.string scanSub.ligand |> JEn.list )
        ]


{-| Processed version of BALS results in the `scan` mode.
-}
type alias AlanineScanResults =
    { name : String
    , pdbFile : String
    , receptor : List ChainID
    , ligand : List ChainID
    , dG : Float
    , receptorResults : List ResidueResult
    , ligandResults : List ResidueResult
    }


{-| BALS results for a specific residue.
-}
type alias ResidueResult =
    { residueNumber : String
    , aminoAcid : String
    , chainID : ChainID
    , ddG : Float
    , residueSize : Int
    , stdDevDDG : Float
    }


{-| Decodes JSON produced by the server into `AlanineScanResults`.
-}
scanResultsDecoder : JDe.Decoder AlanineScanResults
scanResultsDecoder =
    JDe.succeed AlanineScanResults
        |: (JDe.field "name" JDe.string)
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



-- Constellation


{-| Submodel that represents the state for the constellation tab.
-}
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


{-| Union type of the different modes that can be active.
-}
type ConstellationMode
    = Auto AutoSettings


{-| Record containing settings required by BALS auto mode.
-}
type alias AutoSettings =
    { name : String
    , ddGCutOff : String
    , constellationSize : String
    , cutOffDistance : String
    }


defaultAutoSettings : AutoSettings
defaultAutoSettings =
    AutoSettings
        ""
        "5.0"
        "3"
        "13.0"


{-| Validates an `AutoSettings` to determine if it can be safely submitted.
-}
validAutoSettings : AutoSettings -> Bool
validAutoSettings { name, ddGCutOff, constellationSize, cutOffDistance } =
    let
        validName =
            name /= ""

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
            [ validName
            , validDDGCutOff
            , validConSize
            , validDistance
            , not anyEmpty
            ]


{-| True if a string is a valid structure of a floating point number.
-}
representsFloat : String -> Bool
representsFloat inString =
    case String.toFloat inString of
        Ok _ ->
            True

        Err _ ->
            False


{-| True if a string is a valid structure of an integer.
-}
representsInt : String -> Bool
representsInt inString =
    case String.toInt inString of
        Ok _ ->
            True

        Err _ ->
            False


{-| JSON encoder that creates an auto job from a previous `AlanineScanResults`
job and the settings for the auto job selected by the user.
-}
encodeAutoJob : AlanineScanResults -> AutoSettings -> JEn.Value
encodeAutoJob scanResults settings =
    JEn.object
        [ ( "name", settings.name |> JEn.string )
        , ( "ddGCutOff"
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
                    settings.cutOffDistance
                )
                |> JEn.float
          )
        , ( "scanName", scanResults.name |> JEn.string )
        , ( "pdbFile", scanResults.pdbFile |> JEn.string )
        , ( "receptor", List.map JEn.string scanResults.receptor |> JEn.list )
        , ( "ligand", List.map JEn.string scanResults.ligand |> JEn.list )
        ]


{-| Record containing the processed results of a BALS auto job.
-}
type alias ConstellationResults =
    { name : String
    , hotConstellations : List ( String, Float )
    , scanResults : AlanineScanResults
    }


{-| Decodes JSON produced by the server into `ConstellationResults`.
-}
autoResultsDecoder : JDe.Decoder ConstellationResults
autoResultsDecoder =
    JDe.succeed ConstellationResults
        |: (JDe.field "name" JDe.string)
        |: (JDe.field "hotConstellations" <|
                JDe.list <|
                    JDe.map2
                        (,)
                        (JDe.index 0 JDe.string)
                        (JDe.index 1 JDe.float)
           )
        |: (JDe.field "scanResults" scanResultsDecoder)



-- Structure


type alias Structure =
    { pdbFile : String
    , chainLabels : List ChainID
    , hidden : List Bool
    }


type alias ExportableStructure =
    Structure


type MoleculeStyle
    = BallsAndSticks
    | Cartoon
    | Lines
    | Spheres


stringToMoleculeStyle : String -> MoleculeStyle
stringToMoleculeStyle styleString =
    case styleString of
        "BallsAndSticks" ->
            BallsAndSticks

        "Cartoon" ->
            Cartoon

        "Lines" ->
            Lines

        "Spheres" ->
            Spheres

        _ ->
            Lines



-- Job


type alias JobDetails =
    { jobID : String
    , name : String
    , status : JobStatus
    , stdOut : Maybe String
    }


type alias ExportableJobDetails =
    { jobID : String
    , name : String
    , status : String
    , stdOut : Maybe String
    }


exportJobDetails : JobDetails -> ExportableJobDetails
exportJobDetails details =
    { details | status = toString details.status }


importJobDetails : ExportableJobDetails -> JobDetails
importJobDetails exported =
    { exported | status = stringToStatus exported.status }


{-| Decodes JSON produced by the server into `JobDetails`.
-}
jobDetailsDecoder : JDe.Decoder JobDetails
jobDetailsDecoder =
    JDe.succeed JobDetails
        |: (JDe.field "_id" JDe.string)
        |: (JDe.field "name" JDe.string)
        |: (JDe.field "status" (JDe.int |> JDe.andThen intToJobStatus))
        |: (JDe.maybe (JDe.field "std_out" JDe.string))


{-| Represents the possible status that any job on the server could have. This
must match the `JobStatus` enum in database.py.
-}
type JobStatus
    = Submitted
    | Queued
    | Running
    | Completed
    | Failed


stringToStatus : String -> JobStatus
stringToStatus statusString =
    case statusString of
        "Submitted" ->
            Submitted

        "Queued" ->
            Queued

        "Running" ->
            Running

        "Completed" ->
            Completed

        "Failed" ->
            Failed

        _ ->
            Submitted


{-| Converts the integer structure of the status into a `JobStatus`. The
options in this case statement must match the possible options for the
`JobStatus` enum in database.py.
-}
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


{-| Filters a list of `JobDetails` for jobs that are submitted, queued or
running.
-}
getActiveJobs : List JobDetails -> List JobDetails
getActiveJobs jobs =
    List.filter
        (\{ status } -> (status /= Completed) && (status /= Failed))
        jobs



{- # UPDATE
   This section contains all code that updates the state of the application.
-}


{-| Type that signals to the main update function how the model should be
updated.
-}
type Msg
    = SetAppMode AppMode
    | UpdateScan ScanMsg
    | UpdateConstellation ConstellationMsg
    | DismissNotification Int
    | OpenPanel Panel
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
update : Msg -> Model -> ( Model, Cmd Msg )
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
                                    , appMode = Jobs
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
                            , appMode = Scan
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
            let
                ( updatedConModel, conSubCmds, notifications ) =
                    updateConstellation constellationMsg model.constellation
            in
                case constellationMsg of
                    AutoJobSubmitted (Ok _) ->
                        let
                            ( updatedModel, updatedCmds ) =
                                { model
                                    | appMode = Jobs
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

                    ProcessAutoResults (Ok { scanResults }) ->
                        let
                            scanUpdateMsg =
                                ProcessScanResults (Ok scanResults)
                                    |> UpdateScan

                            ( updatedModel, scanCmds ) =
                                update scanUpdateMsg model
                        in
                            { updatedModel
                                | appMode = Constellation
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

                    _ ->
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
                Closed ->
                    { model | openPanel = panel }
                        ! []

                _ ->
                    { model | openPanel = Closed }
                        ! []

        ClosePanel ->
            { model | openPanel = Closed } ! []

        SaveState ->
            model ! [ exportModel model |> saveState ]



-- Scan Mode


{-| Type that signals to `updateScan` how the current `AlanineScanModel` should
be updated.
-}
type ScanMsg
    = GetStructure
    | UpdateStructure Structure
    | ChangeVisibility String
    | SetReceptor ChainID
    | SetLigand ChainID
    | ClearReceptor ChainID
    | ClearLigand ChainID
    | SetScanName String
    | SubmitScanJob
    | ScanJobSubmitted (Result Http.Error JobDetails)
    | CheckScanJobs Time.Time
    | ProcessScanStatus (Result Http.Error JobDetails)
    | GetScanResults String
    | ProcessScanResults (Result Http.Error AlanineScanResults)
    | FocusOnResidue ResidueResult
    | ClearScanSubmission


{-| Function for updating the state of the currently active `AlanineScanModel`.
This includes code for manipulating the structure of the currently active
scan submission structure.
-}
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
                            |> List.map (\job -> ( job.jobID, job ))
                            |> Dict.fromList
                            |> Dict.insert jobDetails.jobID jobDetails
                            |> Dict.toList
                            |> List.map Tuple.second
                }
                    ! []
                    # case jobDetails.status of
                        Completed ->
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

                        Failed ->
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
                { scanModel | results = Nothing } ! [] # []



-- Constellation


{-| Type that signals to `updateConstellation` how the current
`ConstellationModel` should be updated.
-}
type ConstellationMsg
    = ChangeMode String
    | UpdateAutoSettings AutoSettingsMsg
    | SubmitConstellationJob AlanineScanResults
    | AutoJobSubmitted (Result Http.Error JobDetails)
    | CheckAutoJobs Time.Time
    | ProcessAutoJobStatus (Result Http.Error JobDetails)
    | GetAutoResults String
    | ProcessAutoResults (Result Http.Error ConstellationResults)


{-| Function for updating the state of the currently active
`ConstellationModel`.
-}
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
                            |> List.map (\job -> ( job.jobID, job ))
                            |> Dict.fromList
                            |> Dict.insert jobDetails.jobID jobDetails
                            |> Dict.toList
                            |> List.map Tuple.second
                }
                    ! []
                    # case jobDetails.status of
                        Completed ->
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

                        Failed ->
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


type AutoSettingsMsg
    = UpdateName String
    | UpdateDDGCutOff String
    | UpdateSize String
    | UpdateDistanceCutOff String


{-| Small helper update that handles user input for creating AutoSettings`
-}
updateAutoSettings : AutoSettingsMsg -> AutoSettings -> AutoSettings
updateAutoSettings msg settings =
    case msg of
        UpdateName name ->
            { settings | name = name }

        UpdateDDGCutOff ddGValue ->
            { settings | ddGCutOff = ddGValue }

        UpdateSize constellationSize ->
            { settings | constellationSize = constellationSize }

        UpdateDistanceCutOff distance ->
            { settings | cutOffDistance = distance }



{- # COMMANDS AND SUBSCRIPTIONS
   This section contains all code for communicating with the outside world, whether
   that's with the server or javascript inside the index.html file.
-}
{- Each of the following ports corresponds to a JavaScript function in
   index.html. The function is ran when the port command is returned by the update
   function.
-}


port saveState : ExportableModel -> Cmd msg


port initialiseViewer : () -> Cmd msg


port requestPDBFile : () -> Cmd msg


port setVisibility : ( String, Bool ) -> Cmd msg


port colourGeometry : ( String, ChainID ) -> Cmd msg


port displayScanResults : AlanineScanResults -> Cmd msg


port focusOnResidue : ResidueResult -> Cmd msg



{- These functions create commands that create HTTP requests. -}


{-| Submits an alanine scan job to the server.
-}
submitAlanineScan : Structure -> AlanineScanSub -> Cmd ScanMsg
submitAlanineScan structure scanSub =
    Http.send ScanJobSubmitted <|
        Http.post
            "/api/v0.1/alanine-scan-jobs"
            (encodeAlanineScanSub structure scanSub
                |> Http.jsonBody
            )
            jobDetailsDecoder


{-| Checks the status of an alanine scan job on the server.
-}
checkScanJobStatus : JobDetails -> Cmd ScanMsg
checkScanJobStatus { jobID } =
    Http.send ProcessScanStatus <|
        Http.get
            ("/api/v0.1/alanine-scan-job/" ++ jobID ++ "?get-status")
            jobDetailsDecoder


{-| Gets the results of an alanine scan job from the server.
-}
getScanResults : String -> Cmd ScanMsg
getScanResults jobID =
    Http.send ProcessScanResults <|
        Http.get
            ("/api/v0.1/alanine-scan-job/" ++ jobID ++ "?get-results")
            scanResultsDecoder


{-| Submits an alanine scan job to the server. Receives a string back containing
the job id if sucessful.
-}
submitAutoJob : AlanineScanResults -> AutoSettings -> Cmd ConstellationMsg
submitAutoJob scanResults settings =
    Http.send AutoJobSubmitted <|
        Http.post
            "/api/v0.1/auto-jobs"
            (encodeAutoJob scanResults settings
                |> Http.jsonBody
            )
            jobDetailsDecoder


{-| Checks the status of an auto constellation job on the server.
-}
checkAutoJobStatus : JobDetails -> Cmd ConstellationMsg
checkAutoJobStatus { jobID } =
    Http.send ProcessAutoJobStatus <|
        Http.get
            ("/api/v0.1/auto-job/" ++ jobID ++ "?get-status")
            jobDetailsDecoder


{-| Gets the results of an auto constellation job from the server.
-}
getAutoResults : String -> Cmd ConstellationMsg
getAutoResults jobID =
    Http.send ProcessAutoResults <|
        Http.get
            ("/api/v0.1/auto-job/" ++ jobID ++ "?get-results")
            autoResultsDecoder



-- Subscriptions
{- The following ports are triggered by JavaScript functions in index.html and
   send data into the Elm application.
-}


port receiveStructure : (ExportableStructure -> msg) -> Sub msg


port atomClick : (ResidueInfo -> msg) -> Sub msg


{-| Describes the active subscriptions based on the current state of the model.
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch <|
        [ receiveStructure <| UpdateScan << UpdateStructure

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



{- # VIEWS
   This section contains all functions for displaying the current state of the
   model.
-}


{-| The main view function that is called every time the model is updated.
-}
view : Model -> Html Msg
view model =
    div [ class "main-grid" ]
        ([ div [ class "banner" ]
            [ header [] [ h1 [] [ text "BUDE Alanine Scan" ] ]
            , div [ class "controls" ]
                [ div
                    [ class "control-button"
                    , onClick <| OpenPanel ViewerOptions
                    ]
                    [ text "⚛️" ]
                , div
                    [ class "control-button"
                    , onClick <| OpenPanel Notifications
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
                                model.alanineScan.structure
                                model.alanineScan.alanineScanSub

                Constellation ->
                    case model.constellation.results of
                        Just results ->
                            constellationResultsView results

                        Nothing ->
                            constellationSubmissionView
                                UpdateConstellation
                                model.constellation
                                model.alanineScan.results

                Jobs ->
                    jobsView model
           )
         ]
            ++ case model.openPanel of
                Notifications ->
                    [ notificationPanel model.notifications ]

                ViewerOptions ->
                    [ viewerOptions model.alanineScan.structure ]

                Closed ->
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
viewerOptions : Maybe Structure -> Html Msg
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
                            , th [] [ text "Style" ]
                            , th [] [ text "Colour" ]
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


viewerSelectionRow : ( ChainID, Bool ) -> Html Msg
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
    -> Maybe Structure
    -> AlanineScanSub
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
                        , validScanSub scanSub |> not |> disabled
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
scanResultsView : (ScanMsg -> msg) -> AlanineScanResults -> Html msg
scanResultsView updateMsg results =
    let
        resultsRow resResult =
            let
                { chainID, residueNumber, aminoAcid, ddG } =
                    resResult
            in
                tr [ onClick <| updateMsg <| FocusOnResidue resResult ]
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
            , h3 [] [ text "Job Name" ]
            , p [] [ text results.name ]
            , h3 [] [ text "ΔG" ]
            , p [] [ toString results.dG |> text ]
            , h3 [] [ text "Residue Results (Non-Zero)" ]
            , table [ class "scan-results-table" ]
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


{-| View for selecting the receptor and ligand chains.
-}
chainSelect :
    (ScanMsg -> msg)
    -> List ChainID
    -> List ChainID
    -> ChainID
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


{-| Actual submission view for constellation jobs. Only shown if a valid
`AlanineScanResults` is active in the model.
-}
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


{-| View for submission of auto settings input.
-}
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
            ([ text "Job Name"
             , input
                [ onInput <| updateMsg << UpdateAutoSettings << UpdateName
                ]
                []
             , text "ΔΔG Cut Off Value"
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


{-| Main view for the constellation tab when in results mode. This is hidden
in submission mode.
-}
constellationResultsView : ConstellationResults -> Html msg
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
                (List.sortBy (\{ name } -> name) alaScanJobs)
            , jobTable (UpdateConstellation << GetAutoResults)
                "Constellation Scan Jobs"
                (List.sortBy (\{ name } -> name) constellationJobs)
            ]


{-| Creates a table of job details. Takes a `Msg` constructor as an argument so
that it can be reused for all the job queues.
-}
jobTable : (String -> Msg) -> String -> List JobDetails -> Html Msg
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
                table [ class "jobs-table" ]
                    ([ tr []
                        [ th [] [ text "Name" ]
                        , th [] [ text "Job ID" ]
                        , th [] [ text "Status" ]
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


simpleOption : String -> Html msg
simpleOption labelValue =
    option [ value labelValue ] [ text labelValue ]
