module Model exposing
    ( AlanineScanModel
    , AlanineScanResults
    , AlanineScanSub
    , AppMode(..)
    , AutoSettings
    , ChainID
    , ConstellationMode(..)
    , ConstellationModel
    , ConstellationResults
    , JobDetails
    , JobStatus(..)
    , ManualSettings
    , Model
    , Panel(..)
    , ResidueColour
    , ResidueInfo
    , ResidueResult
    , ResiduesSettings
    , SaveState
    , Structure
    , autoResultsDecoder
    , defaultAutoSettings
    , defaultManualSettings
    , defaultResiduesSettings
    , emptyAlaScanModel
    , emptyConstellationModel
    , emptyModel
    , emptyScanSub
    , encodeAlanineScanSub
    , encodeAutoJob
    , encodeManualJob
    , encodeResiduesJob
    , getActiveJobs
    , jobDetailsDecoder
    , loadModel
    , modelToSaveState
    , saveModel
    , saveStateToModel
    , scanResultsDecoder
    , statusToString
    , validAutoSettings
    , validManualSettings
    , validResiduesSettings
    , validScanSub
    )

{- # MODEL
   This module contains the types that describe the state of the application,
   as well functions for creating, validating and encode/decoding them to/from
   JSON.
-}

import Http
import Json.Decode as JDe
import Json.Encode as JEn
import Json.Encode.Extra as JEnEx
import Notifications exposing (Notification, encodeNotification, notificationDecoder)
import RemoteData exposing (RemoteData)
import Set


{-| Describes the current state of the application
-}
type alias Model =
    { appMode : AppMode
    , alanineScan : AlanineScanModel
    , constellation : ConstellationModel
    , hoveredName : Maybe String
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
        Nothing
        []
        Closed


type alias SaveState =
    { alaScanJobs : List JobDetails
    , autoJobs : List JobDetails
    , manualJobs : List JobDetails
    , residuesJobs : List JobDetails
    , notifications : List Notification
    }


modelToSaveState : Model -> SaveState
modelToSaveState model =
    SaveState
        model.alanineScan.jobs
        model.constellation.autoJobs
        model.constellation.manualJobs
        model.constellation.residuesJobs
        model.notifications


saveStateToModel : Maybe SaveState -> Model
saveStateToModel mSaveState =
    case mSaveState of
        Just saveState ->
            Model
                Scan
                { emptyAlaScanModel | jobs = saveState.alaScanJobs }
                { emptyConstellationModel
                    | autoJobs = saveState.autoJobs
                    , manualJobs = saveState.manualJobs
                    , residuesJobs = saveState.residuesJobs
                }
                Nothing
                saveState.notifications
                Closed

        Nothing ->
            emptyModel


encodeSaveState : SaveState -> JEn.Value
encodeSaveState saveState =
    JEn.object
        [ ( "alaScanJobs", JEn.list encodeJobDetails saveState.alaScanJobs )
        , ( "autoJobs", JEn.list encodeJobDetails saveState.autoJobs )
        , ( "manualJobs", JEn.list encodeJobDetails saveState.manualJobs )
        , ( "residuesJobs", JEn.list encodeJobDetails saveState.residuesJobs )
        , ( "notifications"
          , JEn.list encodeNotification
                saveState.notifications
          )
        ]


saveStateDecoder : JDe.Decoder SaveState
saveStateDecoder =
    JDe.map5 SaveState
        (JDe.field "alaScanJobs" (JDe.list jobDetailsDecoder))
        (JDe.field "autoJobs" (JDe.list jobDetailsDecoder))
        (JDe.field "manualJobs" (JDe.list jobDetailsDecoder))
        (JDe.field "residuesJobs" (JDe.list jobDetailsDecoder))
        (JDe.field "notifications" (JDe.list notificationDecoder))


saveModel : Model -> JEn.Value
saveModel =
    modelToSaveState >> encodeSaveState


loadModel : JDe.Value -> Result JDe.Error Model
loadModel value =
    JDe.decodeValue (JDe.nullable saveStateDecoder) value
        |> Result.map saveStateToModel


{-| Modes available in the app, corresponding to the 3 main tabs.
-}
type AppMode
    = Scan
    | Constellation
    | Jobs



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
    , rotamerFixActive : Bool
    , submissionRequest : RemoteData Http.Error JobDetails
    }


emptyScanSub : AlanineScanSub
emptyScanSub =
    AlanineScanSub
        ""
        []
        []
        True
        RemoteData.NotAsked


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
        , ( "receptor", JEn.list JEn.string scanSub.receptor )
        , ( "ligand", JEn.list JEn.string scanSub.ligand )
        , ( "rotamerFixActive", JEn.bool scanSub.rotamerFixActive )
        ]


{-| Processed version of BAlaS results in the `scan` mode.
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


{-| BAlaS results for a specific residue.
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
    JDe.map7 AlanineScanResults
        (JDe.field "name" JDe.string)
        (JDe.field "pdbFile" JDe.string)
        (JDe.field "receptor" (JDe.list JDe.string))
        (JDe.field "ligand" (JDe.list JDe.string))
        (JDe.field "dG" JDe.float)
        (JDe.field "receptorData" <|
            JDe.list <|
                JDe.map6 ResidueResult
                    (JDe.index 0 JDe.string)
                    (JDe.index 1 JDe.string)
                    (JDe.index 2 JDe.string)
                    (JDe.index 3 JDe.float)
                    (JDe.index 4 JDe.int)
                    (JDe.index 5 JDe.float)
        )
        (JDe.field "ligandData" <|
            JDe.list <|
                JDe.map6 ResidueResult
                    (JDe.index 0 JDe.string)
                    (JDe.index 1 JDe.string)
                    (JDe.index 2 JDe.string)
                    (JDe.index 3 JDe.float)
                    (JDe.index 4 JDe.int)
                    (JDe.index 5 JDe.float)
        )



-- Constellation


{-| Submodel that represents the state for the constellation tab.
-}
type alias ConstellationModel =
    { constellationSub : ConstellationMode
    , submissionRequest : RemoteData Http.Error JobDetails
    , autoJobs : List JobDetails
    , manualJobs : List JobDetails
    , residuesJobs : List JobDetails
    , results : Maybe ConstellationResults
    }


emptyConstellationModel : ConstellationModel
emptyConstellationModel =
    ConstellationModel
        (Manual defaultManualSettings)
        RemoteData.NotAsked
        []
        []
        []
        Nothing


{-| Union type of the different modes that can be active.
-}
type ConstellationMode
    = Auto AutoSettings
    | Manual ManualSettings
    | Residues ResiduesSettings


{-| Record containing settings required by BAlaS auto mode.
-}
type alias AutoSettings =
    { name : String
    , ddGCutOff : String
    , constellationSize : String
    , cutOffDistance : String
    , rotamerFixActive : Bool
    }


defaultAutoSettings : AutoSettings
defaultAutoSettings =
    AutoSettings
        ""
        "5.0"
        "3"
        "13.0"
        True


{-| Validates an `AutoSettings` to determine if it can be safely submitted.
-}
validAutoSettings : List ResidueResult -> AutoSettings -> Bool
validAutoSettings ligandResults { name, ddGCutOff, constellationSize, cutOffDistance } =
    let
        ddGCO =
            ddGCutOff
                |> String.toFloat
                |> Maybe.withDefault 0

        constSize =
            constellationSize
                |> String.toInt
                |> Maybe.withDefault 0

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

        numResMoreThanConstSize =
            ligandResults
                |> List.filter (\res -> res.ddG > ddGCO)
                |> List.length
                |> (\x -> x > constSize)
    in
    List.all identity
        [ validName
        , validDDGCutOff
        , validConSize
        , validDistance
        , not anyEmpty
        , numResMoreThanConstSize
        ]


{-| JSON encoder that creates an auto job from a previous `AlanineScanResults`
job and the settings for the auto job selected by the user.
-}
encodeAutoJob : AlanineScanResults -> AutoSettings -> JEn.Value
encodeAutoJob scanResults settings =
    JEn.object
        [ ( "name", settings.name |> JEn.string )
        , ( "ddGCutOff"
          , Maybe.withDefault
                5.0
                (String.toFloat
                    settings.ddGCutOff
                )
                |> JEn.float
          )
        , ( "constellationSize"
          , Maybe.withDefault
                3
                (String.toInt
                    settings.constellationSize
                )
                |> JEn.int
          )
        , ( "cutOffDistance"
          , Maybe.withDefault
                13.0
                (String.toFloat
                    settings.cutOffDistance
                )
                |> JEn.float
          )
        , ( "scanName", JEn.string scanResults.name )
        , ( "pdbFile", JEn.string scanResults.pdbFile )
        , ( "receptor", JEn.list JEn.string scanResults.receptor )
        , ( "ligand", JEn.list JEn.string scanResults.ligand )
        , ( "rotamerFixActive", JEn.bool settings.rotamerFixActive )
        ]


{-| Record containing settings required by BAlaS manual mode.
-}
type alias ManualSettings =
    { name : String
    , residues : Set.Set String
    , rotamerFixActive : Bool
    }


defaultManualSettings : ManualSettings
defaultManualSettings =
    ManualSettings
        ""
        Set.empty
        True


{-| Validates an `ManualSettings` to determine if it can be safely submitted.
-}
validManualSettings : ManualSettings -> Bool
validManualSettings { name, residues } =
    let
        validName =
            name /= ""

        validResidues =
            Set.isEmpty residues |> not
    in
    List.all identity
        [ validName
        , validResidues
        ]


{-| JSON encoder that creates an manual job from a previous `AlanineScanResults`
job and the settings for the manual job selected by the user.
-}
encodeManualJob : AlanineScanResults -> ManualSettings -> JEn.Value
encodeManualJob scanResults { name, residues, rotamerFixActive } =
    JEn.object
        [ ( "name", JEn.string name )
        , ( "residues"
          , Set.toList residues
                |> JEn.list JEn.string
          )
        , ( "scanName", scanResults.name |> JEn.string )
        , ( "pdbFile", scanResults.pdbFile |> JEn.string )
        , ( "receptor", JEn.list JEn.string scanResults.receptor )
        , ( "ligand", JEn.list JEn.string scanResults.ligand )
        , ( "rotamerFixActive", JEn.bool rotamerFixActive )
        ]


{-| Record containing settings required by BAlaS residues mode.
-}
type alias ResiduesSettings =
    { name : String
    , constellationSize : Int
    , residues : Set.Set String
    , rotamerFixActive : Bool
    }


defaultResiduesSettings : ResiduesSettings
defaultResiduesSettings =
    ResiduesSettings
        ""
        3
        Set.empty
        True


{-| Validates an `ResiduesSettings` to determine if it can be safely submitted.
-}
validResiduesSettings : ResiduesSettings -> Bool
validResiduesSettings { name, constellationSize, residues } =
    let
        validName =
            name /= ""

        validConstSize =
            constellationSize > 1

        validResidues =
            (Set.isEmpty residues |> not)
                && (Set.size residues >= constellationSize)
    in
    List.all identity
        [ validName
        , validConstSize
        , validResidues
        ]


{-| JSON encoder that creates an residues job from a previous `AlanineScanResults`
job and the settings for the residues job selected by the user.
-}
encodeResiduesJob : AlanineScanResults -> ResiduesSettings -> JEn.Value
encodeResiduesJob scanResults { name, constellationSize, residues, rotamerFixActive } =
    JEn.object
        [ ( "name", JEn.string name )
        , ( "constellationSize", JEn.int constellationSize )
        , ( "residues"
          , Set.toList residues
                |> JEn.list JEn.string
          )
        , ( "scanName", JEn.string scanResults.name )
        , ( "pdbFile", JEn.string scanResults.pdbFile )
        , ( "receptor", JEn.list JEn.string scanResults.receptor )
        , ( "ligand", JEn.list JEn.string scanResults.ligand )
        , ( "rotamerFixActive", JEn.bool rotamerFixActive )
        ]


{-| True if a string is a valid structure of a floating point number.
-}
representsFloat : String -> Bool
representsFloat inString =
    case String.toFloat inString of
        Just _ ->
            True

        Nothing ->
            False


{-| True if a string is a valid structure of an integer.
-}
representsInt : String -> Bool
representsInt inString =
    case String.toInt inString of
        Just _ ->
            True

        Nothing ->
            False


{-| Record containing the processed results of a BAlaS auto job.
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
    JDe.map3 ConstellationResults
        (JDe.field "name" JDe.string)
        (JDe.field "hotConstellations" <|
            JDe.list <|
                JDe.map2
                    (\a b -> ( a, b ))
                    (JDe.index 0 JDe.string)
                    (JDe.index 1 JDe.float)
        )
        (JDe.field "scanResults" scanResultsDecoder)



-- Structure


type alias Structure =
    { pdbFile : String
    , chainLabels : List ChainID
    , geometryLabels : List ( String, Bool )
    }


type alias ResidueColour =
    { geometryLabel : String
    , residues : List ( ChainID, String )
    , colour : String
    }



-- Job


type alias JobDetails =
    { jobID : String
    , name : String
    , status : JobStatus
    , stdOut : Maybe String
    }


encodeJobDetails : JobDetails -> JEn.Value
encodeJobDetails { jobID, name, status, stdOut } =
    JEn.object
        [ ( "_id", JEn.string jobID )
        , ( "name", JEn.string name )
        , ( "status", jobStatusToInt status |> JEn.int )
        , ( "stdOut", JEnEx.maybe JEn.string stdOut )
        ]


{-| Decodes JSON produced by the server into `JobDetails`.
-}
jobDetailsDecoder : JDe.Decoder JobDetails
jobDetailsDecoder =
    JDe.map4 JobDetails
        (JDe.field "_id" JDe.string)
        (JDe.field "name" JDe.string)
        (JDe.field "status" (JDe.int |> JDe.andThen intToJobStatus))
        (JDe.maybe (JDe.field "std_out" JDe.string))


{-| Represents the possible status that any job on the server could have. This
must match the `JobStatus` enum in database.py.
-}
type JobStatus
    = Submitted
    | Queued
    | Running
    | Completed
    | Failed


statusToString : JobStatus -> String
statusToString jobStatus =
    case jobStatus of
        Submitted ->
            "Submitted"

        Queued ->
            "Queued"

        Running ->
            "Running"

        Completed ->
            "Completed"

        Failed ->
            "Failed"


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


jobStatusToInt : JobStatus -> Int
jobStatusToInt status =
    case status of
        Submitted ->
            1

        Queued ->
            2

        Running ->
            3

        Completed ->
            4

        Failed ->
            5


{-| Filters a list of `JobDetails` for jobs that are submitted, queued or
running.
-}
getActiveJobs : List JobDetails -> List JobDetails
getActiveJobs jobs =
    List.filter
        (\{ status } -> (status /= Completed) && (status /= Failed))
        jobs
