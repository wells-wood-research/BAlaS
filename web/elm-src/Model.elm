module Model exposing (..)

{- # MODEL
   This module contains the types that describe the state of the application,
   as well functions for creating, validating and encode/decoding them to/from
   JSON.
-}

import Set
import Json.Decode as JDe
import Json.Decode.Extra exposing ((|:))
import Json.Encode as JEn
import Notifications exposing ((#), Notification)


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


type alias ExportableModel =
    { alanineScan : List ExportableJobDetails
    , autoJobs : List ExportableJobDetails
    , manualJobs : List ExportableJobDetails
    , notifications : List Notification
    }


exportModel : Model -> ExportableModel
exportModel model =
    ExportableModel
        (List.map exportJobDetails model.alanineScan.jobs)
        (List.map exportJobDetails model.constellation.autoJobs)
        (List.map exportJobDetails model.constellation.manualJobs)
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
            | autoJobs =
                List.map importJobDetails exported.autoJobs
            , manualJobs =
                List.map importJobDetails exported.manualJobs
        }
        Nothing
        exported.notifications
        Closed


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
    , autoJobs : List JobDetails
    , manualJobs : List JobDetails
    , results : Maybe ConstellationResults
    }


emptyConstellationModel : ConstellationModel
emptyConstellationModel =
    ConstellationModel
        (Auto defaultAutoSettings)
        []
        []
        Nothing


{-| Union type of the different modes that can be active.
-}
type ConstellationMode
    = Auto AutoSettings
    | Manual ManualSettings


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


{-| Record containing settings required by BALS manual mode.
-}
type alias ManualSettings =
    { name : String
    , residues : Set.Set String
    }


defaultManualSettings : ManualSettings
defaultManualSettings =
    ManualSettings
        ""
        Set.empty


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


{-| JSON encoder that creates an manual job from a previous `AlanineScanResults`
job and the settings for the manual job selected by the user.
-}
encodeManualJob : AlanineScanResults -> ManualSettings -> JEn.Value
encodeManualJob scanResults { name, residues } =
    JEn.object
        [ ( "name", JEn.string name )
        , ( "residues"
          , Set.toList residues
                |> List.map JEn.string
                |> JEn.list
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
    , geometryLabels : List ( String, Bool )
    }


type alias ExportableStructure =
    Structure


type alias ResidueColour =
    { geometryLabel : String
    , chain : String
    , residueNumber : String
    , colour : String
    }



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
