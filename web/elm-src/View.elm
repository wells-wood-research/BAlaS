module View exposing (sessionView)

{-| The main view function that is called every time the model is updated.
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model
import Notifications exposing (Notification)
import Regex
import Session
import Set
import Update



{- # VIEW
   This section contains all functions for displaying the current state of the
   model.
-}


sessionView : Session.Session -> Html Update.Msg
sessionView session =
    case session of
        Session.ActiveMode model _ ->
            view model


view : Model.Model -> Html Update.Msg
view model =
    div [ class "main-grid" ]
        ([ div [ class "banner" ]
            [ header [] [ h1 [] [ text "BUDE Alanine Scan" ] ]
            , div [ class "controls" ]
                [ div
                    [ class "control-button"
                    , onClick <| Update.OpenPanel Model.ViewerOptions
                    ]
                    [ text "⚛️" ]
                , div
                    [ class "control-button"
                    , onClick <| Update.OpenPanel Model.Notifications
                    ]
                    [ List.length model.notifications
                        |> String.fromInt
                        |> (++) "🔔"
                        |> text
                    ]
                ]
            ]
         , div [ id "viewer" ]
            [ div [ class "hover-label" ]
                [ Maybe.withDefault "" model.hoveredName |> text ]
            ]
         , div [ class "tabs" ]
            [ div
                [ class "tab scan-tab"
                , onClick <| Update.SetAppMode Model.Scan
                ]
                [ text "Scan" ]
            , div
                [ class "tab constellation-tab"
                , onClick <|
                    Update.SetAppMode Model.Constellation
                ]
                [ text "Constellation" ]
            , div
                [ class "tab jobs-tab"
                , onClick <| Update.SetAppMode Model.Jobs
                ]
                [ text "Jobs" ]
            ]
         , case model.appMode of
            Model.Scan ->
                case model.alanineScan.results of
                    Just results ->
                        scanResultsView results

                    Nothing ->
                        scanSubmissionView
                            Update.UpdateScan
                            model.alanineScan.structure
                            model.alanineScan.alanineScanSub

            Model.Constellation ->
                case model.constellation.results of
                    Just results ->
                        constellationResultsView results

                    Nothing ->
                        constellationSubmissionView
                            Update.UpdateConstellation
                            model.constellation
                            model.alanineScan.results

            Model.Jobs ->
                jobsView model
         ]
            ++ (case model.openPanel of
                    Model.Notifications ->
                        [ notificationPanel model.notifications ]

                    Model.ViewerOptions ->
                        [ viewerOptions model.alanineScan.structure ]

                    Model.Closed ->
                        []
               )
        )


{-| Side panel that displays notifications. Can be toggled on and off.
-}
notificationPanel : List Notification -> Html Update.Msg
notificationPanel notifications =
    div [ class "notification-panel" ]
        [ h3 [] [ text "Notifications" ]
        , button [ onClick <| Update.ClearNotifications ] [ text "Dismiss All" ]
        , button [ onClick Update.ClosePanel ] [ text "Close" ]
        , div [] <| List.indexedMap notificationView notifications
        ]


{-| Displays an individual notification.
-}
notificationView : Int -> Notification -> Html Update.Msg
notificationView idx notification =
    div [ class "notification" ]
        [ h4 [] [ text notification.title ]
        , button [ onClick <| Update.DismissNotification idx ] [ text "Dismiss" ]
        , p [ class "details" ]
            [ text notification.message ]
        ]


{-| Side panel that displays viewer options.
-}
viewerOptions : Maybe Model.Structure -> Html Update.Msg
viewerOptions mStructure =
    div [ class "notification-panel" ]
        [ h3 [] [ text "Viewer Options" ]
        , button [ onClick Update.ClosePanel ] [ text "Close" ]
        , case mStructure of
            Just structure ->
                div []
                    [ table []
                        ([ tr []
                            [ th [] [ text "Selection" ]
                            , th [] [ text "Hidden" ]
                            ]
                         ]
                            ++ (structure.geometryLabels
                                    |> List.map viewerSelectionRow
                               )
                        )
                    ]

            Nothing ->
                div []
                    [ text "Load a structure before changing structure." ]
        ]


viewerSelectionRow : ( String, Bool ) -> Html Update.Msg
viewerSelectionRow ( geometryLabel, hidden ) =
    tr []
        [ td [] [ text geometryLabel ]
        , td []
            [ input
                [ onClick <|
                    Update.UpdateScan <|
                        Update.ChangeVisibility geometryLabel
                , type_ "checkbox"
                , checked hidden
                ]
                []
            ]
        ]



-- Scan


{-| Main view for the scan tab when in submission mode. This is hidden in
results mode.
-}
scanSubmissionView :
    (Update.ScanMsg -> msg)
    -> Maybe Model.Structure
    -> Model.AlanineScanSub
    -> Html msg
scanSubmissionView updateMsg mStructure scanSub =
    div
        [ class "control-panel scan-panel" ]
        [ h2 [] [ text "Scan Submission" ]
        , div []
            [ input [ type_ "file", id "pdbFileToLoad" ] []
            , button [ onClick <| updateMsg Update.GetStructure ] [ text "Upload" ]
            ]
        , div []
            (case mStructure of
                Just structure ->
                    [ div []
                        [ text "Job Name"
                        , br [] []
                        , input [ onInput <| updateMsg << Update.SetScanName ] []
                        ]
                    , table []
                        ([ tr []
                            [ th [] [ text "Chain" ]
                            , th [] [ text "Receptor" ]
                            , th [] [ text "Ligand" ]
                            ]
                         ]
                            ++ List.map
                                (chainSelect updateMsg
                                    scanSub.receptor
                                    scanSub.ligand
                                )
                                structure.chainLabels
                        )
                    , button
                        [ onClick <| updateMsg Update.SubmitScanJob
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
scanResultsView : Model.AlanineScanResults -> Html Update.Msg
scanResultsView results =
    div
        [ class "control-panel scan-panel" ]
        [ h2 [] [ text "Alanine Scan Results" ]
        , button
            [ onClick <| Update.UpdateScan Update.ClearScanSubmission ]
            [ text "New Submission" ]
        , h3 [] [ text "Job Name" ]
        , p [] [ text results.name ]
        , h3 [] [ text "ΔG" ]
        , p [] [ String.fromFloat results.dG |> text ]
        , h3 [] [ text "Residue Results (Non-Zero)" ]
        , text
            "Standard deviation only available for multiple models (NMR, MD etc)."
        , scanResultsTable results.ligandResults
        ]


scanResultsTable :
    List Model.ResidueResult
    -> Html Update.Msg
scanResultsTable ligandResults =
    let
        scanResultsRow resResult =
            let
                { chainID, residueNumber, aminoAcid, ddG, stdDevDDG } =
                    resResult
            in
            tr
                [ onClick <| Update.UpdateScan <| Update.FocusOnResidue resResult
                , Model.ResidueColour
                    "ligand_ballsAndSticks"
                    [ ( chainID
                      , residueNumber
                      )
                    ]
                    "red"
                    |> Update.ColourResidues
                    |> onMouseOver
                , Model.ResidueColour
                    "ligand_ballsAndSticks"
                    [ ( chainID
                      , residueNumber
                      )
                    ]
                    "cpk"
                    |> Update.ColourResidues
                    |> onMouseOut
                ]
                [ td [] [ text chainID ]
                , td [] [ text residueNumber ]
                , td [] [ text aminoAcid ]
                , td [] [ String.fromFloat ddG |> text ]
                , td [] [ String.fromFloat stdDevDDG |> text ]
                ]
    in
    table [ class "scan-results-table" ]
        ([ tr []
            [ th [] [ text "Chain" ]
            , th [] [ text "Residue" ]
            , th [] [ text "Amino Acid" ]
            , th [] [ text "ΔΔG" ]
            , th [] [ text "Std Dev" ]
            ]
         ]
            ++ (List.filter (\res -> res.ddG /= 0) ligandResults
                    |> List.map scanResultsRow
               )
        )


{-| View for selecting the receptor and ligand chains.
-}
chainSelect :
    (Update.ScanMsg -> msg)
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
                            Update.ClearReceptor label
                    ]
                    [ text "Clear" ]
                ]

             else if List.member label ligandLabels then
                []

             else
                [ button
                    [ onClick <|
                        updateMsg <|
                            Update.SetReceptor label
                    ]
                    [ text "Set" ]
                ]
            )
        , td []
            (if List.member label ligandLabels then
                [ button
                    [ onClick <|
                        updateMsg <|
                            Update.ClearLigand label
                    ]
                    [ text "Clear" ]
                ]

             else if List.member label receptorLabels then
                []

             else
                [ button
                    [ onClick <|
                        updateMsg <|
                            Update.SetLigand label
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
    (Update.ConstellationMsg -> msg)
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
    (Update.ConstellationMsg -> msg)
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

                Model.Residues _ ->
                    "Residues"
    in
    div []
        [ h3 [] [ text "Select Mode" ]
        , select [ onInput <| updateMsg << Update.ChangeMode ] <|
            List.map (simpleOption modeString)
                [ "Auto", "Manual", "Residues" ]
        , case model.constellationSub of
            Model.Auto settings ->
                autoSettingsView updateMsg scanRes settings

            Model.Manual settings ->
                manualSettingsView updateMsg scanRes settings

            Model.Residues settings ->
                residuesSettingsView updateMsg scanRes settings
        ]


{-| View for submission of auto settings input.
-}
autoSettingsView :
    (Update.ConstellationMsg -> msg)
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
            [ onInput <|
                updateMsg
                    << Update.UpdateAutoSettings
                    << Update.UpdateAutoName
            ]
            []
        , h3 [] [ text "ΔΔG Cut Off Value" ]
        , input
            [ onInput <|
                updateMsg
                    << Update.UpdateAutoSettings
                    << Update.UpdateDDGCutOff
            , pattern "[+-]?([0-9]*[.])?[0-9]+"
            , value ddGCutOff
            , placeholder "ΔΔG"
            ]
            []
        , h3 [] [ text "Constellation Size" ]
        , input
            [ onInput <|
                updateMsg
                    << Update.UpdateAutoSettings
                    << Update.UpdateSize
            , pattern "[0-9]*"
            , value constellationSize
            , placeholder "Size"
            ]
            []
        , h3 [] [ text "Cut Off Distance" ]
        , input
            [ onInput <|
                updateMsg
                    << Update.UpdateAutoSettings
                    << Update.UpdateDistanceCutOff
            , pattern "[+-]?([0-9]*[.])?[0-9]+"
            , value cutOffDistance
            , placeholder "Distance"
            ]
            []
        , br [] []
        , button
            [ onClick <| updateMsg <| Update.SubmitConstellationJob scanRes
            , disabled <| not <| Model.validAutoSettings settings
            ]
            [ text "Submit" ]
        ]


{-| View for submission of manual settings input.
-}
manualSettingsView :
    (Update.ConstellationMsg -> msg)
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
                    << Update.UpdateManualSettings
                    << Update.UpdateManualName
            ]
            []
        , h3 [] [ text "Select Residues" ]
        , text "Click to select."
        , residueSelectTable
            (updateMsg
                << Update.UpdateManualSettings
                << Update.SelectManualResidue
            )
            residues
            scanResults.ligandResults
        , br [] []
        , button
            [ onClick <| updateMsg <| Update.SubmitConstellationJob scanResults
            , disabled <| not <| Model.validManualSettings settings
            ]
            [ text "Submit" ]
        ]


residueSelectTable :
    (Model.ResidueResult -> msg)
    -> Set.Set String
    -> List Model.ResidueResult
    -> Html msg
residueSelectTable updateMsg selected ligandResults =
    let
        residueResultsRow resResult =
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
                    updateMsg resResult
                ]
                [ td [] [ text chainID ]
                , td [] [ text residueNumber ]
                , td [] [ text aminoAcid ]
                , td [] [ String.fromFloat ddG |> text ]
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
                    |> List.map residueResultsRow
               )
        )


{-| View for submission of residues settings input.
-}
residuesSettingsView :
    (Update.ConstellationMsg -> msg)
    -> Model.AlanineScanResults
    -> Model.ResiduesSettings
    -> Html msg
residuesSettingsView updateMsg scanResults settings =
    let
        { residues } =
            settings
    in
    div []
        [ h3 [] [ text "Job Name" ]
        , input
            [ onInput <|
                updateMsg
                    << Update.UpdateResiduesSettings
                    << Update.UpdateResiduesName
            ]
            []
        , h3 [] [ text "Constellation Size" ]
        , select
            [ onInput <|
                updateMsg
                    << Update.UpdateResiduesSettings
                    << Update.UpdateConstellationSize
            ]
          <|
            List.map (simpleOption <| String.fromInt settings.constellationSize)
                [ "2", "3", "4", "5" ]
        , h3 [] [ text "Select Residues" ]
        , text "Click to select."
        , residueSelectTable
            (updateMsg
                << Update.UpdateResiduesSettings
                << Update.SelectResiduesResidue
            )
            residues
            scanResults.ligandResults
        , br [] []
        , button
            [ onClick <| updateMsg <| Update.SubmitConstellationJob scanResults
            , disabled <| not <| Model.validResiduesSettings settings
            ]
            [ text "Submit" ]
        ]


{-| Main view for the constellation tab when in results mode. This is hidden
in submission mode.
-}
constellationResultsView : Model.ConstellationResults -> Html Update.Msg
constellationResultsView { hotConstellations } =
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


resultsRow : ( String, Float ) -> Html Update.Msg
resultsRow ( constellation, meanDDG ) =
    let
        constResidues =
            Regex.find
                (Regex.fromString "([A-Za-z])(\\d+)"
                    |> Maybe.withDefault Regex.never
                )
                constellation
                |> List.filterMap submatchToMaybe
    in
    tr
        [ Model.ResidueColour
            "ligand_ballsAndSticks"
            constResidues
            "red"
            |> Update.ColourResidues
            |> onMouseOver
        , Model.ResidueColour
            "ligand_ballsAndSticks"
            constResidues
            "cpk"
            |> Update.ColourResidues
            |> onMouseOut
        ]
        [ td [] [ text constellation ]
        , td [] [ text <| String.fromFloat meanDDG ]
        ]


submatchToMaybe : Regex.Match -> Maybe ( String, String )
submatchToMaybe match =
    case match.submatches of
        [ Just chain, Just resNumber ] ->
            Just ( chain, resNumber )

        _ ->
            Nothing



-- Jobs


{-| Main view for the Jobs tabs.
-}
jobsView : Model.Model -> Html Update.Msg
jobsView model =
    let
        alaScanJobs =
            model.alanineScan.jobs

        autoJobs =
            model.constellation.autoJobs

        manualJobs =
            model.constellation.manualJobs

        residuesJobs =
            model.constellation.residuesJobs
    in
    div [ class "control-panel jobs-panel" ]
        [ h2 [] [ text "Jobs" ]
        , jobTable
            (Update.UpdateScan << Update.GetScanResults)
            (Update.UpdateScan << Update.DeleteScanJob)
            "Alanine Scan Jobs"
            (List.sortBy (\{ name } -> name) alaScanJobs)
        , jobTable
            (Update.UpdateConstellation << Update.GetAutoResults)
            (Update.UpdateConstellation << Update.DeleteAutoJob)
            "Auto Constellation Scan Jobs"
            (List.sortBy (\{ name } -> name) autoJobs)
        , jobTable
            (Update.UpdateConstellation << Update.GetManualResults)
            (Update.UpdateConstellation << Update.DeleteManualJob)
            "Manual Constellation Scan Jobs"
            (List.sortBy (\{ name } -> name) manualJobs)
        , jobTable
            (Update.UpdateConstellation << Update.GetResiduesResults)
            (Update.UpdateConstellation << Update.DeleteResiduesJob)
            "Residues Constellation Scan Jobs"
            (List.sortBy (\{ name } -> name) residuesJobs)
        ]


{-| Creates a table of job details. Takes a `Msg` constructor as an argument so
that it can be reused for all the job queues.
-}
jobTable :
    (String -> Update.Msg)
    -> (String -> Update.Msg)
    -> String
    -> List Model.JobDetails
    -> Html Update.Msg
jobTable getMsg deleteMsg tableTitle jobs =
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
                    ++ List.map (jobTableRow getMsg deleteMsg) jobs
                )

          else
            text "No Jobs submitted."
        ]


jobTableRow :
    (String -> Update.Msg)
    -> (String -> Update.Msg)
    -> Model.JobDetails
    -> Html Update.Msg
jobTableRow getMsg deleteMsg { jobID, name, status } =
    tr [ class "details" ]
        [ td [] [ text name ]
        , td [] [ text jobID ]
        , td [] [ text <| Model.statusToString status ]
        , td []
            [ button
                [ getMsg jobID
                    |> onClick
                , (if status == Model.Completed then
                    False

                   else
                    True
                  )
                    |> disabled
                ]
                [ text "Get Results" ]
            ]
        , td []
            [ button
                [ deleteMsg jobID
                    |> onClick
                ]
                [ text "Delete" ]
            ]
        ]



-- HELPER FUNCTIONS


simpleOption : String -> String -> Html msg
simpleOption selectedLabel labelValue =
    case selectedLabel == labelValue of
        True ->
            option [ value labelValue, selected True ] [ text labelValue ]

        False ->
            option [ value labelValue ] [ text labelValue ]
