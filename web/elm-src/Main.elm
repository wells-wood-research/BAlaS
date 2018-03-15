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


emptyModel : Model
emptyModel =
    Model
        ScanSubmission
        emptyAlaScanModel
        []
        False


type alias ChainID =
    String


type alias AlanineScanModel =
    { alanineScanSub : AlanineScanSub
    , jobs : List JobDetails
    }


emptyAlaScanModel : AlanineScanModel
emptyAlaScanModel =
    AlanineScanModel emptyScanSub []


type alias AlanineScanSub =
    { pdbFile : Maybe String
    , chainVisibility : Maybe (Dict.Dict ChainID Bool)
    , receptor : Maybe ChainID
    , ligand : Maybe ChainID
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
        isJust pdbFile && isJust receptor && isJust ligand


emptyScanSub : AlanineScanSub
emptyScanSub =
    AlanineScanSub
        Nothing
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
                { model
                    | alanineScan =
                        updatedScanModel
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
    | ClearReceptorLigand
    | SubmitScanJob
    | ScanJobSubmitted (Result Http.Error JobDetails)
    | CheckScanJobs Time.Time
    | ProcessScanStatus (Result Http.Error JobDetails)


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
                                    , ligand = Nothing
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
                                    | receptor = Just chainID
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
                                    | ligand = Just chainID
                                }
                        }
                            ! [ colourGeometry ( "blue", chainID ) ]
                            # []

                    Nothing ->
                        scanModel ! [] # []

            ClearReceptorLigand ->
                case scanSub.pdbFile of
                    Just pdb ->
                        { scanModel
                            | alanineScanSub =
                                { scanSub
                                    | receptor = Nothing
                                    , ligand = Nothing
                                }
                        }
                            ! [ showStructure pdb ]
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
                    # []

            ProcessScanStatus (Err error) ->
                let
                    err =
                        Debug.log "Scan status check error" error
                in
                    scanModel ! [] # []


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
        Http.get ("/api/v0.1/alanine-scan-job/" ++ jobID) jobDetailsDecoder



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


chainSelect :
    (ScanMsg -> msg)
    -> Maybe ChainID
    -> Maybe ChainID
    -> ( ChainID, Bool )
    -> Html msg
chainSelect updateMsg mReceptorLabel mLigandLabel ( label, visible ) =
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
            (case mReceptorLabel of
                Just receptorLabel ->
                    if receptorLabel == label then
                        [ button [ onClick <| updateMsg ClearReceptorLigand ]
                            [ text "Clear" ]
                        ]
                    else
                        []

                Nothing ->
                    let
                        recButton =
                            button
                                [ onClick <|
                                    updateMsg <|
                                        SetReceptor label
                                ]
                                [ text "Set" ]
                    in
                        case mLigandLabel of
                            Just ligandLabel ->
                                if ligandLabel == label then
                                    []
                                else
                                    [ recButton ]

                            Nothing ->
                                [ recButton ]
            )
        , td []
            (case mLigandLabel of
                Just ligandLabel ->
                    if ligandLabel == label then
                        [ button [ onClick <| updateMsg ClearReceptorLigand ]
                            [ text "Clear" ]
                        ]
                    else
                        []

                Nothing ->
                    let
                        ligButton =
                            button [ onClick <| updateMsg <| SetLigand label ]
                                [ text "Set" ]
                    in
                        case mReceptorLabel of
                            Just receptorLabel ->
                                if receptorLabel == label then
                                    []
                                else
                                    [ ligButton ]

                            Nothing ->
                                [ ligButton ]
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
