module View exposing (view)

{-| The main view function that is called every time the model is updated.
-}

import Css
import Fancy
import Html
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model
import Notifications exposing (Notification)
import Regex
import Set
import Update



{- # VIEW
   This section contains all functions for displaying the current state of the
   model.
-}


view : Model.Model -> Html Update.Msg
view model =
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "2fr 1fr"
            , Css.property "grid-template-rows" "5% 95%"
            , Css.height (Css.pct 100)
            , Css.left Css.zero
            , Css.minWidth (Css.px 500)
            , Css.position Css.absolute
            , Css.top Css.zero
            , Css.width (Css.pct 100)
            ]
        ]
        ([ banner <| List.length model.notifications
         , viewer model.hoveredName
         , modeTabs model
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
            ++ sidePanel model
        )


banner : Int -> Html Update.Msg
banner notificationNumber =
    div
        [ css
            [ Css.alignItems Css.center
            , Css.backgroundColor Fancy.colourPalette.c2
            , Css.displayFlex
            , Css.property "grid-column" "1 / 2"
            , Css.property "grid-row" "1 / 2"
            , Css.justifyContent Css.spaceBetween
            , Css.padding (Css.px 5)
            ]
        ]
        [ header [] [ Fancy.h1 [] [ text "BUDE Alanine Scan" ] ]
        , div [ css [ Css.displayFlex ] ]
            [ Fancy.controlButton
                [ onClick <| Update.OpenPanel Model.ViewerOptions
                ]
                [ text "⚛️" ]
            , Fancy.controlButton
                [ onClick <| Update.OpenPanel Model.Notifications
                ]
                [ notificationNumber
                    |> String.fromInt
                    |> (++) "🔔"
                    |> text
                ]
            ]
        ]


viewer : Maybe String -> Html msg
viewer hoveredName =
    div
        [ id "viewer"
        , css
            [ Css.property "grid-column" "1 / 2"
            , Css.property "grid-row" "2 / 3"
            , Css.overflow Css.hidden
            , Css.position Css.relative
            ]
        ]
        [ hoverLabel hoveredName
        ]


hoverLabel : Maybe String -> Html msg
hoverLabel hoveredName =
    div
        [ css
            [ Css.backgroundColor Fancy.colourPalette.white
            , Css.padding (Css.px 2)
            , Css.position Css.absolute
            , Css.zIndex (Css.int 100)
            ]
        ]
        [ Maybe.withDefault "" hoveredName |> text ]


modeTabs : Model.Model -> Html Update.Msg
modeTabs model =
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "repeat(3, 1fr)"
            , Css.property "grid-column" "2 / 3"
            , Css.property "grid-row" "1 / 2"
            , Css.textAlign Css.center
            , Css.hover
                [ Css.cursor Css.pointer ]
            ]
        ]
        [ tab
            [ css [ Css.backgroundColor Fancy.colourPalette.c3 ]
            , onClick <| Update.SetAppMode Model.Scan
            ]
            [ text "Scan" ]
        , tab
            [ css [ Css.backgroundColor Fancy.colourPalette.c4 ]
            , onClick <|
                Update.SetAppMode Model.Constellation
            ]
            [ text "Constellation" ]
        , tab
            [ css [ Css.backgroundColor Fancy.colourPalette.c5 ]
            , onClick <| Update.SetAppMode Model.Jobs
            ]
            [ text "Jobs" ]
        ]


tab : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
tab =
    styled button
        [ Css.alignItems Css.center
        , Css.displayFlex
        , Css.justifyContent Css.center
        , Css.border Css.zero
        , Css.focus
            [ Css.outline Css.zero
            ]
        ]


sidePanel : Model.Model -> List (Html Update.Msg)
sidePanel model =
    case model.openPanel of
        Model.Notifications ->
            [ notificationPanel model.notifications ]

        Model.ViewerOptions ->
            [ viewerOptions model.alanineScan.structure ]

        Model.Closed ->
            []


panel : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
panel =
    styled div
        [ Css.backgroundColor Fancy.colourPalette.c2
        , Css.boxShadow5
            Css.zero
            (Css.px 5)
            (Css.px 10)
            Css.zero
            (Css.rgba 133 133 133 1)
        , Css.height (Css.pct 100)
        , Css.left Css.zero
        , Css.position Css.fixed
        , Css.textAlign Css.center
        , Css.top Css.zero
        , Css.width (Css.px 300)
        , Css.zIndex (Css.int 1000)
        ]


{-| Side panel that displays notifications. Can be toggled on and off.
-}
notificationPanel : List Notification -> Html Update.Msg
notificationPanel notifications =
    panel
        []
        [ Fancy.h3 [] [ text "Notifications" ]
        , Fancy.button [ onClick <| Update.ClearNotifications ] [ text "Dismiss All" ]
        , Fancy.button [ onClick Update.ClosePanel ] [ text "Close" ]
        , div [] <| List.indexedMap notificationView notifications
        ]


{-| Displays an individual notification.
-}
notificationView : Int -> Notification -> Html Update.Msg
notificationView idx notification =
    div
        [ css
            [ Css.backgroundColor Fancy.colourPalette.c3
            , Css.margin (Css.px 3)
            , Css.padding (Css.px 3)
            , Css.textAlign Css.left
            ]
        ]
        [ Fancy.h4 [] [ text notification.title ]
        , Fancy.button [ onClick <| Update.DismissNotification idx ] [ text "Dismiss" ]
        , details [] [ text notification.message ]
        ]


{-| Side panel that displays viewer options.
-}
viewerOptions : Maybe Model.Structure -> Html Update.Msg
viewerOptions mStructure =
    panel
        []
        [ Fancy.h3 [] [ text "Viewer Options" ]
        , Fancy.button [ onClick Update.ClosePanel ] [ text "Close" ]
        , case mStructure of
            Just structure ->
                Fancy.table []
                    ([ Fancy.tr []
                        [ Fancy.th [] [ text "Selection" ]
                        , Fancy.th [] [ text "Hidden" ]
                        ]
                     ]
                        ++ (structure.geometryLabels
                                |> List.map viewerSelectionRow
                           )
                    )

            Nothing ->
                div []
                    [ text "Load a structure before changing the representation." ]
        ]


viewerSelectionRow : ( String, Bool ) -> Html Update.Msg
viewerSelectionRow ( geometryLabel, hidden ) =
    Fancy.tr []
        [ Fancy.td [] [ text geometryLabel ]
        , Fancy.td []
            [ Fancy.input
                [ onClick <|
                    Update.UpdateScan <|
                        Update.ChangeVisibility geometryLabel
                , type_ "checkbox"
                , checked hidden
                ]
                []
            ]
        ]


tabPane : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
tabPane =
    styled div
        [ Css.property "grid-column" "2 / 3"
        , Css.property "grid-row" "2 / 3"
        , Css.padding (Css.px 10)
        , Css.overflow Css.auto
        , Css.maxWidth (Css.pct 100)
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
    tabPane
        [ css
            [ Css.backgroundColor Fancy.colourPalette.c3
            ]
        ]
        [ Fancy.h2 [] [ text "Scan Submission" ]
        , div []
            [ Fancy.input [ type_ "file", id "pdbFileToLoad" ] []
            , Fancy.button [ onClick <| updateMsg Update.GetStructure ] [ text "Upload" ]
            ]
        , div []
            (case mStructure of
                Just structure ->
                    [ div []
                        [ text "Job Name"
                        , br [] []
                        , Fancy.input
                            [ onInput <| updateMsg << Update.SetScanName
                            , value <| scanSub.name
                            ]
                            []
                        ]
                    , Fancy.table []
                        ([ Fancy.tr []
                            [ Fancy.th [] [ text "Chain" ]
                            , Fancy.th [] [ text "Receptor" ]
                            , Fancy.th [] [ text "Ligand" ]
                            ]
                         ]
                            ++ List.map
                                (chainSelect updateMsg
                                    scanSub.receptor
                                    scanSub.ligand
                                )
                                structure.chainLabels
                        )
                    , Fancy.button
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
    tabPane
        [ css
            [ Css.backgroundColor Fancy.colourPalette.c3
            ]
        ]
        [ Fancy.h2 [] [ text "Alanine Scan Results" ]
        , Fancy.button
            [ onClick <| Update.UpdateScan Update.ClearScanSubmission ]
            [ text "New Submission" ]
        , Fancy.h3 [] [ text "Job Name" ]
        , Fancy.p [] [ text results.name ]
        , Fancy.h3 [] [ text "ΔG" ]
        , Fancy.p [] [ String.fromFloat results.dG |> text ]
        , Fancy.h3 [] [ text "Residue Results (Non-Zero)" ]
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
            Fancy.tr
                [ onClick <|
                    Update.UpdateScan <|
                        Update.FocusOnResidue resResult
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
                [ Fancy.td [] [ text chainID ]
                , Fancy.td [] [ text residueNumber ]
                , Fancy.td [] [ text aminoAcid ]
                , Fancy.td [] [ String.fromFloat ddG |> text ]
                , Fancy.td [] [ String.fromFloat stdDevDDG |> text ]
                ]
    in
    Fancy.table
        [ css
            [ Css.tableLayout Css.fixed
            , Css.width (Css.pct 100)
            ]
        ]
        ([ Fancy.tr []
            [ Fancy.th [] [ text "Chain" ]
            , Fancy.th [] [ text "Residue" ]
            , Fancy.th [] [ text "Amino Acid" ]
            , Fancy.th [] [ text "ΔΔG" ]
            , Fancy.th [] [ text "Std Dev" ]
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
    Fancy.tr []
        [ Fancy.td [] [ text label ]
        , Fancy.td []
            (if List.member label receptorLabels then
                [ Fancy.button
                    [ onClick <|
                        updateMsg <|
                            Update.ClearReceptor label
                    ]
                    [ text "Clear" ]
                ]

             else if List.member label ligandLabels then
                []

             else
                [ Fancy.button
                    [ onClick <|
                        updateMsg <|
                            Update.SetReceptor label
                    ]
                    [ text "Set" ]
                ]
            )
        , Fancy.td []
            (if List.member label ligandLabels then
                [ Fancy.button
                    [ onClick <|
                        updateMsg <|
                            Update.ClearLigand label
                    ]
                    [ text "Clear" ]
                ]

             else if List.member label receptorLabels then
                []

             else
                [ Fancy.button
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
    tabPane
        [ css
            [ Css.backgroundColor Fancy.colourPalette.c4
            ]
        ]
        [ Fancy.h2 [] [ text "Constellation Submission" ]
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
        [ Fancy.h3 [] [ text "Select Mode" ]
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
        [ Fancy.h3 [] [ text "Job Name" ]
        , Fancy.input
            [ onInput <|
                updateMsg
                    << Update.UpdateAutoSettings
                    << Update.UpdateAutoName
            ]
            []
        , Fancy.h3 [] [ text "ΔΔG Cut Off Value" ]
        , Fancy.input
            [ onInput <|
                updateMsg
                    << Update.UpdateAutoSettings
                    << Update.UpdateDDGCutOff
            , pattern "[+-]?([0-9]*[.])?[0-9]+"
            , value ddGCutOff
            , placeholder "ΔΔG"
            ]
            []
        , Fancy.h3 [] [ text "Constellation Size" ]
        , Fancy.input
            [ onInput <|
                updateMsg
                    << Update.UpdateAutoSettings
                    << Update.UpdateSize
            , pattern "[0-9]*"
            , value constellationSize
            , placeholder "Size"
            ]
            []
        , Fancy.h3 [] [ text "Cut Off Distance" ]
        , Fancy.input
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
        , Fancy.button
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
        [ Fancy.h3 [] [ text "Job Name" ]
        , Fancy.input
            [ onInput <|
                updateMsg
                    << Update.UpdateManualSettings
                    << Update.UpdateManualName
            ]
            []
        , Fancy.h3 [] [ text "Select Residues" ]
        , text "Click to select."
        , residueSelectTable
            (updateMsg
                << Update.UpdateManualSettings
                << Update.SelectManualResidue
            )
            residues
            scanResults.ligandResults
        , br [] []
        , Fancy.button
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
            Fancy.tr
                [ case Set.member residueID selected of
                    True ->
                        class "selected-residue"

                    False ->
                        class "unselected-residue"
                , onClick <|
                    updateMsg resResult
                ]
                [ Fancy.td [] [ text chainID ]
                , Fancy.td [] [ text residueNumber ]
                , Fancy.td [] [ text aminoAcid ]
                , Fancy.td [] [ String.fromFloat ddG |> text ]
                ]
    in
    Fancy.table []
        ([ Fancy.tr []
            [ Fancy.th [] [ text "Chain" ]
            , Fancy.th [] [ text "Residue" ]
            , Fancy.th [] [ text "Amino Acid" ]
            , Fancy.th [] [ text "ΔΔG" ]
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
        [ Fancy.h3 [] [ text "Job Name" ]
        , Fancy.input
            [ onInput <|
                updateMsg
                    << Update.UpdateResiduesSettings
                    << Update.UpdateResiduesName
            ]
            []
        , Fancy.h3 [] [ text "Constellation Size" ]
        , select
            [ onInput <|
                updateMsg
                    << Update.UpdateResiduesSettings
                    << Update.UpdateConstellationSize
            ]
          <|
            List.map (simpleOption <| String.fromInt settings.constellationSize)
                [ "2", "3", "4", "5" ]
        , Fancy.h3 [] [ text "Select Residues" ]
        , text "Click to select."
        , residueSelectTable
            (updateMsg
                << Update.UpdateResiduesSettings
                << Update.SelectResiduesResidue
            )
            residues
            scanResults.ligandResults
        , br [] []
        , Fancy.button
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
    tabPane
        [ css
            [ Css.backgroundColor Fancy.colourPalette.c4
            ]
        ]
        [ Fancy.h2 [] [ text "Constellation Scan Results" ]
        , Fancy.h3 [] [ text "🔥 Hot Constellations 🔥" ]
        , Fancy.table []
            ([ Fancy.tr []
                [ Fancy.th [] [ text "Constellation" ]
                , Fancy.th [] [ text "Mean ΔΔG" ]
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
    Fancy.tr
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
        [ Fancy.td [] [ text constellation ]
        , Fancy.td [] [ text <| String.fromFloat meanDDG ]
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
    tabPane
        [ css
            [ Css.backgroundColor Fancy.colourPalette.c5
            ]
        ]
        [ Fancy.h2 [] [ text "Jobs" ]
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
        [ Fancy.h3 [] [ text tableTitle ]
        , if List.length jobs > 0 then
            Fancy.table []
                ([ Fancy.tr []
                    [ Fancy.th [] [ text "Name" ]
                    , Fancy.th [] [ text "Job ID" ]
                    , Fancy.th [] [ text "Status" ]
                    , Fancy.th [] []
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
    Fancy.tr
        [ css
            [ Css.fontSize (Css.pt 10)
            ]
        ]
        [ Fancy.td [] [ text name ]
        , Fancy.td [] [ text jobID ]
        , Fancy.td [] [ text <| Model.statusToString status ]
        , Fancy.td []
            [ Fancy.button
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
        , Fancy.td []
            [ Fancy.button
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
