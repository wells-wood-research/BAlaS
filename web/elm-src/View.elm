module View exposing (view)

{-| The main view function that is called every time the model is updated.
-}

import Css
import Fancy
import Html
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as JDe
import Markdown
import Model
import Notifications exposing (Notification)
import Regex
import RemoteData
import Set
import Update
import Url.Builder as UrlB



{- # VIEW
   This section contains all functions for displaying the current state of the
   model.
-}


view : Model.Model -> Html Update.Msg
view model =
    div
        [ css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "3fr 2fr"
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
                            model.alanineScan.structure
                            model.alanineScan.alanineScanSub
                            model.settings

            Model.Constellation ->
                case model.constellation.results of
                    Just results ->
                        constellationResultsView results

                    Nothing ->
                        constellationSubmissionView
                            model.constellation
                            model.alanineScan.results
                            model.settings

            Model.Jobs ->
                jobsView model
         ]
            ++ (case model.alanineScan.structure of
                    Just _ ->
                        []

                    Nothing ->
                        [ welcomeWindow ]
               )
            ++ sidePanel model
        )


welcomeWindow : Html msg
welcomeWindow =
    div
        [ css
            [ Css.alignItems Css.center
            , Css.displayFlex
            , Css.height (Css.pct 95)
            , Css.justifyContent Css.center
            , Css.left Css.zero
            , Css.position Css.absolute
            , Css.top (Css.pct 5)
            , Css.width (Css.pct 60)
            , Css.zIndex (Css.int 800)
            ]
        ]
        [ div
            [ css
                [ Css.backgroundColor Fancy.colourPalette.c2
                , Css.maxHeight (Css.pct 100)
                , Css.margin Css.zero |> Css.important
                , Css.overflow Css.auto
                , Css.padding (Css.px 5)
                , Css.width (Css.pct 60)
                ]
            ]
            [ Fancy.h2 [] [ text "BUDE Alanine Scan" ]
            , Fancy.h3 []
                [ text
                    ("Powerful, interactive and user-friendly "
                        ++ "computational alanine scanning."
                    )
                ]
            , Fancy.h4 []
                [ text "Version 1.0, 2018-10-09. "
                , a [ href "https://github.com/woolfson-group/balas" ]
                    [ text "Source code" ]
                , text "."
                ]
            , Fancy.hr [] []
            , b [] [ text "THE APP CITATION WILL GO HERE" ]
            , br [] []
            , b [] [ text "THE BAlaS COMMANDLINE CITATION WILL GO HERE" ]
            , Fancy.p []
                [ """
                To get started go to the Scan tab and upload your structure
                file. If this is your first time using BUDE Alanine Scan, you
                might want to run through the tutorial, which you can access
                using the "❓" button on the bottom right."""
                    |> text
                ]
            ]
        ]


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
            , Fancy.controlButton
                [ onClick <| Update.OpenPanel Model.SettingsMenu
                ]
                [ text "⚙️" ]
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

        Model.SettingsMenu ->
            [ settingsMenu model.settings ]

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


{-| Side panel that displays settings options.
-}
settingsMenu : Model.Settings -> Html Update.Msg
settingsMenu settings =
    panel
        []
        [ Fancy.h3 [] [ text "Settings" ]
        , Fancy.button [ onClick Update.ClosePanel ] [ text "Close" ]
        , Fancy.table []
            [ Fancy.tr []
                [ Fancy.td []
                    [ div []
                        [ Fancy.h4 [] [ text "Rotamer Fix" ]
                        , p []
                            [ text """Compensates for conformational entropy loss in charged
                        residues (DERKH). Default: On"""
                            ]
                        ]
                    ]
                , Fancy.td []
                    [ Fancy.input
                        [ onClick <| Update.ToggleRotamerFix settings
                        , type_ "checkbox"
                        , checked settings.rotamerFixActive
                        ]
                        []
                    ]
                ]
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


onChange : msg -> Attribute msg
onChange message =
    on "change" (JDe.succeed message)


onSelectOption : (String -> msg) -> Attribute msg
onSelectOption handler =
    on "change" <| JDe.map handler <| JDe.at [ "target", "value" ] JDe.string


{-| Main view for the scan tab when in submission mode. This is hidden in
results mode.
-}
scanSubmissionView :
    Maybe Model.Structure
    -> Model.AlanineScanSub
    -> Model.Settings
    -> Html Update.Msg
scanSubmissionView mStructure scanSub settings =
    tabPane
        [ css
            [ Css.backgroundColor Fancy.colourPalette.c3
            ]
        ]
        ([ Fancy.h2 [] [ text "Scan Submission" ]
         ]
            ++ (case scanSub.submissionRequest of
                    RemoteData.NotAsked ->
                        submissionForm mStructure scanSub settings

                    RemoteData.Loading ->
                        [ div [] [ text "Submitting job..." ] ]

                    RemoteData.Failure err ->
                        submissionForm mStructure scanSub settings
                            ++ [ div []
                                    [ text
                                        """An error occurred while submitting your job
                                        to the server. Please check your internet
                                        connection and try resubmitting. If the problem
                                        persists, there might be an issue with the
                                        server."""
                                    ]
                               ]

                    RemoteData.Success jobDetails ->
                        [ div []
                            [ text
                                ("Job submitted sucessfully. Job ID: "
                                    ++ jobDetails.jobID
                                )
                            ]
                        ]
               )
        )


submissionForm :
    Maybe Model.Structure
    -> Model.AlanineScanSub
    -> Model.Settings
    -> List (Html Update.Msg)
submissionForm mStructure scanSub settings =
    [ div []
        [ text "Structure Upload"
        , br [] []
        , Fancy.input
            [ type_ "file"
            , onChange <| Update.UpdateScan Update.GetStructure
            , id "pdbFileToLoad"
            ]
            []
        ]
    , div []
        (case mStructure of
            Just structure ->
                [ div []
                    [ text "Job Name"
                    , br [] []
                    , Fancy.input
                        [ onInput <| Update.UpdateScan << Update.SetScanName
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
                            (chainSelect
                                scanSub.receptor
                                scanSub.ligand
                            )
                            structure.chainLabels
                    )
                , Fancy.button
                    [ Update.SubmitScanJob scanSub mStructure settings.rotamerFixActive
                        |> Update.UpdateScan
                        |> onClick
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
        , Fancy.h3 [] [ text "ΔG (J/mol)" ]
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
                    "green"
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
                , Fancy.td []
                    [ roundToXDecPlaces 1 ddG
                        |> String.fromFloat
                        |> text
                    ]
                , Fancy.td []
                    [ roundToXDecPlaces 1 stdDevDDG
                        |> String.fromFloat
                        |> text
                    ]
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
            , Fancy.th [] [ text "ΔΔG (J/mol)" ]
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
    List Model.ChainID
    -> List Model.ChainID
    -> Model.ChainID
    -> Html Update.Msg
chainSelect receptorLabels ligandLabels label =
    Fancy.tr []
        [ Fancy.td [] [ text label ]
        , Fancy.td []
            (if List.member label receptorLabels then
                [ Fancy.button
                    [ onClick <|
                        Update.UpdateScan <|
                            Update.ClearReceptor label
                    ]
                    [ text "Clear" ]
                ]

             else if List.member label ligandLabels then
                []

             else
                [ Fancy.button
                    [ onClick <|
                        Update.UpdateScan <|
                            Update.SetReceptor label
                    ]
                    [ text "Set" ]
                ]
            )
        , Fancy.td []
            (if List.member label ligandLabels then
                [ Fancy.button
                    [ onClick <|
                        Update.UpdateScan <|
                            Update.ClearLigand label
                    ]
                    [ text "Clear" ]
                ]

             else if List.member label receptorLabels then
                []

             else
                [ Fancy.button
                    [ onClick <|
                        Update.UpdateScan <|
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
    Model.ConstellationModel
    -> Maybe Model.AlanineScanResults
    -> Model.Settings
    -> Html Update.Msg
constellationSubmissionView model mScanResults settings =
    tabPane
        [ css
            [ Css.backgroundColor Fancy.colourPalette.c4
            ]
        ]
        [ Fancy.h2 [] [ text "Constellation Submission" ]
        , case mScanResults of
            Just results ->
                case model.submissionRequest of
                    RemoteData.NotAsked ->
                        activeConstellationSub model results settings

                    RemoteData.Loading ->
                        div [] [ text "Submitting job..." ]

                    RemoteData.Failure err ->
                        div []
                            ([ activeConstellationSub model results settings ]
                                ++ [ div []
                                        [ text
                                            """An error occurred while submitting your job
                                            to the server. Please check your internet
                                            connection and try resubmitting. If the problem
                                            persists, there might be an issue with the
                                            server."""
                                        ]
                                   ]
                            )

                    RemoteData.Success jobDetails ->
                        div []
                            [ text
                                ("Job submitted sucessfully. Job ID: "
                                    ++ jobDetails.jobID
                                )
                            ]

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
    Model.ConstellationModel
    -> Model.AlanineScanResults
    -> Model.Settings
    -> Html Update.Msg
activeConstellationSub model scanRes settings =
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
        , select [ onSelectOption <| Update.UpdateConstellation << Update.ChangeMode ] <|
            List.map (simpleOption modeString)
                [ "Auto", "Manual", "Residues" ]
        , case model.constellationSub of
            Model.Auto autoSettings ->
                autoSettingsView scanRes autoSettings settings

            Model.Manual manualSettings ->
                manualSettingsView scanRes manualSettings settings

            Model.Residues residuesSettings ->
                residuesSettingsView scanRes residuesSettings settings
        ]


{-| View for submission of auto settings input.
-}
autoSettingsView :
    Model.AlanineScanResults
    -> Model.AutoSettings
    -> Model.Settings
    -> Html Update.Msg
autoSettingsView scanRes autoSettings settings =
    let
        { name, ddGCutOff, constellationSize, cutOffDistance } =
            autoSettings
    in
    div []
        [ Fancy.h3 [] [ text "Job Name" ]
        , Fancy.input
            [ onInput <|
                Update.UpdateConstellation
                    << Update.UpdateAutoSettings
                    << Update.UpdateAutoName
            , value name
            ]
            []
        , Fancy.h3 [] [ text "ΔΔG Cut Off Value" ]
        , Fancy.input
            [ onInput <|
                Update.UpdateConstellation
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
                Update.UpdateConstellation
                    << Update.UpdateAutoSettings
                    << Update.UpdateSize
            , pattern "[0-9]*"
            , value constellationSize
            , placeholder "Size"
            ]
            []
        , Fancy.h3 [] [ text "Cut Off Distance (Å)" ]
        , Fancy.input
            [ onInput <|
                Update.UpdateConstellation
                    << Update.UpdateAutoSettings
                    << Update.UpdateDistanceCutOff
            , pattern "[+-]?([0-9]*[.])?[0-9]+"
            , value cutOffDistance
            , placeholder "Distance"
            ]
            []
        , br [] []
        , Fancy.button
            [ onClick <|
                Update.UpdateConstellation <|
                    Update.SubmitConstellationJob
                        scanRes
                        (Model.Auto autoSettings)
                        settings.rotamerFixActive
            , disabled <|
                not <|
                    Model.validAutoSettings
                        scanRes.ligandResults
                        autoSettings
            ]
            [ text "Submit" ]
        , meetsCriteriaTable
            autoSettings
            scanRes.ligandResults
        ]


meetsCriteriaTable : Model.AutoSettings -> List Model.ResidueResult -> Html msg
meetsCriteriaTable settings ligandResults =
    let
        ddGCutOff =
            settings.ddGCutOff
                |> String.toFloat
                |> Maybe.withDefault 0

        residueResultsRow resResult =
            let
                { chainID, residueNumber, aminoAcid, ddG } =
                    resResult

                residueID =
                    chainID ++ residueNumber
            in
            Fancy.tr
                []
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
            , Fancy.th [] [ text "ΔΔG (J/mol)" ]
            ]
         ]
            ++ (List.filter (\res -> res.ddG /= 0) ligandResults
                    |> List.filter (\res -> res.ddG > ddGCutOff)
                    |> List.map residueResultsRow
               )
        )


{-| View for submission of manual settings input.
-}
manualSettingsView :
    Model.AlanineScanResults
    -> Model.ManualSettings
    -> Model.Settings
    -> Html Update.Msg
manualSettingsView scanResults manualSettings settings =
    let
        { residues, name } =
            manualSettings
    in
    div []
        [ Fancy.h3 [] [ text "Job Name" ]
        , Fancy.input
            [ onInput <|
                Update.UpdateConstellation
                    << Update.UpdateManualSettings
                    << Update.UpdateManualName
            , value name
            ]
            []
        , Fancy.h3 [] [ text "Select Residues" ]
        , text "Click to select."
        , residueSelectTable
            (Update.UpdateConstellation
                << Update.UpdateManualSettings
                << Update.SelectManualResidue
            )
            residues
            scanResults.ligandResults
        , br [] []
        , Fancy.button
            [ onClick <|
                Update.UpdateConstellation <|
                    Update.SubmitConstellationJob
                        scanResults
                        (Model.Manual manualSettings)
                        settings.rotamerFixActive
            , disabled <| not <| Model.validManualSettings manualSettings
            ]
            [ text "Submit" ]
        ]


residueSelectTable :
    (Model.ResidueResult -> Update.Msg)
    -> Set.Set String
    -> List Model.ResidueResult
    -> Html Update.Msg
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
                ([ onClick <| updateMsg resResult
                 ]
                    ++ (case Set.member residueID selected of
                            True ->
                                [ css
                                    [ Css.backgroundColor (Css.hex "00ff00")
                                        |> Css.important
                                    ]
                                ]

                            False ->
                                []
                       )
                )
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
            , Fancy.th [] [ text "ΔΔG (J/mol)" ]
            ]
         ]
            ++ (List.filter (\res -> res.ddG /= 0) ligandResults
                    |> List.map residueResultsRow
               )
        )


{-| View for submission of residues settings input.
-}
residuesSettingsView :
    Model.AlanineScanResults
    -> Model.ResiduesSettings
    -> Model.Settings
    -> Html Update.Msg
residuesSettingsView scanResults residueSettings settings =
    let
        { residues, name } =
            residueSettings
    in
    div []
        [ Fancy.h3 [] [ text "Job Name" ]
        , Fancy.input
            [ onInput <|
                Update.UpdateConstellation
                    << Update.UpdateResiduesSettings
                    << Update.UpdateResiduesName
            , value name
            ]
            []
        , Fancy.h3 [] [ text "Constellation Size" ]
        , select
            [ onSelectOption <|
                Update.UpdateConstellation
                    << Update.UpdateResiduesSettings
                    << Update.UpdateConstellationSize
            ]
          <|
            List.map (simpleOption <| String.fromInt residueSettings.constellationSize)
                [ "2", "3", "4", "5" ]
        , Fancy.h3 [] [ text "Select Residues" ]
        , text "Click to select."
        , residueSelectTable
            (Update.UpdateConstellation
                << Update.UpdateResiduesSettings
                << Update.SelectResiduesResidue
            )
            residues
            scanResults.ligandResults
        , br [] []
        , Fancy.button
            [ onClick <|
                Update.UpdateConstellation <|
                    Update.SubmitConstellationJob
                        scanResults
                        (Model.Residues residueSettings)
                        settings.rotamerFixActive
            , disabled <| not <| Model.validResiduesSettings residueSettings
            ]
            [ text "Submit" ]
        ]


{-| Main view for the constellation tab when in results mode. This is hidden
in submission mode.
-}
constellationResultsView : Model.ConstellationResults -> Html Update.Msg
constellationResultsView { hotConstellations, scanResults } =
    tabPane
        [ css
            [ Css.backgroundColor Fancy.colourPalette.c4
            ]
        ]
        [ Fancy.h2 [] [ text "Constellation Scan Results" ]
        , Fancy.button
            [ onClick <|
                Update.UpdateConstellation <|
                    Update.ClearResults
            ]
            [ text "Clear Results" ]
        , Fancy.h3 [] [ text "🔥 Hot Constellations 🔥" ]
        , Fancy.table []
            ([ Fancy.tr []
                [ Fancy.th [] [ text "Constellation" ]
                , Fancy.th [ css [ Css.width (Css.pct 20) ] ]
                    [ text "Constellation ΔΔG (J/mol)" ]
                , Fancy.th [ css [ Css.width (Css.pct 20) ] ]
                    [ text "Summed Individual ΔΔGs (J/mol)" ]
                , Fancy.th [ css [ Css.width (Css.pct 20) ] ]
                    [ text "Cooperativity (J/mol)" ]
                ]
             ]
                ++ (hotConstellations
                        |> List.sortBy Tuple.first
                        |> List.map (resultsRow scanResults)
                   )
            )
        ]


resultsRow : Model.AlanineScanResults -> ( String, Float ) -> Html Update.Msg
resultsRow alaScanResults ( constellation, meanDG ) =
    let
        ddG =
            meanDG - alaScanResults.dG

        constResidues =
            Regex.find
                (Regex.fromString "([A-Za-z])(\\d+)"
                    |> Maybe.withDefault Regex.never
                )
                constellation
                |> List.filterMap submatchToMaybe

        residueResults =
            List.filter
                (\resResult ->
                    List.member
                        ( resResult.chainID, resResult.residueNumber )
                        constResidues
                )
                alaScanResults.ligandResults

        residueDetails =
            List.map
                (\{ chainID, residueNumber, aminoAcid } ->
                    chainID
                        ++ residueNumber
                        ++ aminoAcid
                )
                residueResults

        individualDDGSum =
            residueResults
                |> List.map .ddG
                |> List.foldl (+) 0
    in
    Fancy.tr
        [ Model.ResidueColour
            "ligand_ballsAndSticks"
            constResidues
            "green"
            |> Update.ColourResidues
            |> onMouseOver
        , Model.ResidueColour
            "ligand_ballsAndSticks"
            constResidues
            "cpk"
            |> Update.ColourResidues
            |> onMouseOut
        ]
        [ Fancy.td []
            [ details []
                [ summary [] [ text constellation ]
                , Fancy.p [] [ String.join ", " residueDetails |> text ]
                ]
            ]
        , Fancy.td []
            [ text <|
                String.fromFloat <|
                    roundToXDecPlaces 1 ddG
            ]
        , Fancy.td []
            [ text <|
                String.fromFloat <|
                    roundToXDecPlaces 1 individualDDGSum
            ]
        , Fancy.td []
            [ text <|
                String.fromFloat <|
                    roundToXDecPlaces 1 (ddG - individualDDGSum)
            ]
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
            "Alanine Scan Jobs"
            "scan"
            (List.sortBy (\{ name } -> name) alaScanJobs)
        , jobTable
            "Auto Constellation Scan Jobs"
            "auto"
            (List.sortBy (\{ name } -> name) autoJobs)
        , jobTable
            "Manual Constellation Scan Jobs"
            "manual"
            (List.sortBy (\{ name } -> name) manualJobs)
        , jobTable
            "Residues Constellation Scan Jobs"
            "residues"
            (List.sortBy (\{ name } -> name) residuesJobs)
        ]


{-| Creates a table of job details. Takes a `Msg` constructor as an argument so
that it can be reused for all the job queues.
-}
jobTable :
    String
    -> String
    -> List Model.JobDetails
    -> Html Update.Msg
jobTable tableTitle jobRoot jobs =
    div []
        [ Fancy.h3 [] [ text tableTitle ]
        , if List.length jobs > 0 then
            Fancy.table [ css [ Css.width (Css.pct 100) ] ]
                ([ Fancy.tr []
                    [ Fancy.th [ css [ Css.width (Css.pct 20) ] ]
                        [ text "Name" ]
                    , Fancy.th [ css [ Css.width (Css.pct 20) ] ]
                        [ text "Job ID" ]
                    , Fancy.th [ css [ Css.width (Css.pct 20) ] ]
                        [ text "Status" ]
                    , Fancy.th []
                        [ text "Actions" ]
                    ]
                 ]
                    ++ List.map (jobTableRow jobRoot) jobs
                )

          else
            text "No Jobs submitted."
        ]


jobTableRow : String -> Model.JobDetails -> Html Update.Msg
jobTableRow jobRoot { jobID, name, status } =
    Fancy.tr
        [ css
            [ Css.fontSize (Css.pt 10)
            ]
        ]
        [ Fancy.td [] [ text name ]
        , Fancy.td [] [ text jobID ]
        , Fancy.td [] [ text <| Model.statusToString status ]
        , Fancy.td []
            [ ul [ css [ Css.padding (Css.px 5), Css.margin Css.zero ] ]
                ((if status == Model.Completed then
                    [ li []
                        [ a
                            [ href <| UrlB.absolute [ jobRoot, jobID ] [] ]
                            [ text "Show Results" ]
                        ]
                    , li []
                        [ a
                            [ href <|
                                UrlB.absolute [ jobRoot, jobID ]
                                    [ UrlB.string "action" "copy-to-clipboard" ]
                            ]
                            [ text "Copy Results Link to Clipboard" ]
                        ]
                    , li []
                        [ a
                            [ href <|
                                UrlB.absolute
                                    [ "balas-result-files"
                                    , jobID ++ ".zip"
                                    ]
                                    []
                            , download ""
                            ]
                            [ text "Download Full Output" ]
                        ]
                    ]

                  else
                    []
                 )
                    ++ [ li []
                            [ a
                                [ href <|
                                    UrlB.absolute [ jobRoot, jobID ]
                                        [ UrlB.string "action" "delete" ]
                                ]
                                [ text "Delete" ]
                            ]
                       ]
                )
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


roundToXDecPlaces : Int -> Float -> Float
roundToXDecPlaces precision num =
    let
        scaling =
            10 ^ precision |> toFloat
    in
    round (num * scaling)
        |> toFloat
        |> (\x -> x / scaling)
