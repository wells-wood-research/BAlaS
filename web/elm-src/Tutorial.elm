module Tutorial exposing
    ( Config
    , Section
    , Tutorial(..)
    , moveBackwards
    , moveForward
    , tutorial
    )

import Css
import DemoData exposing (pdb1ycr)
import Fancy
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Markdown
import Model
    exposing
        ( AlanineScanSub
        , JobDetails
        , emptyAlaScanModel
        , emptyConstellationModel
        , emptyModel
        )
import Ports
import RemoteData
import Set
import Update


type Tutorial msg
    = Tutorial (Sections msg) (CurrentSection msg) (Sections msg)


type alias Sections msg =
    List (Section msg)


type alias CurrentSection msg =
    Section msg


type alias Section msg =
    { tutorialWindow : Html msg
    , model : Model.Model
    , command : Cmd Update.Msg
    }


type alias Config msg =
    { previous : msg
    , next : msg
    , cancel : msg
    }


moveForward : Tutorial msg -> Tutorial msg
moveForward (Tutorial previousSections currentSection nextSections) =
    case List.head nextSections of
        Just section ->
            Tutorial
                (currentSection :: previousSections)
                section
                (List.tail nextSections
                    |> Maybe.withDefault []
                )

        Nothing ->
            Tutorial previousSections currentSection nextSections


moveBackwards : Tutorial msg -> Tutorial msg
moveBackwards (Tutorial previousSections currentSection nextSections) =
    case List.head previousSections of
        Just section ->
            Tutorial
                (List.tail previousSections
                    |> Maybe.withDefault []
                )
                section
                (currentSection :: nextSections)

        Nothing ->
            Tutorial previousSections currentSection nextSections


tutorial : Config msg -> Tutorial msg
tutorial config =
    Tutorial [] (welcome config) <|
        List.map (\s -> s config)
            [ modes
            , gettingStarted
            , theViewer
            , scanSubmission
            , theJobsPanel
            , scanResults
            , constellationBasics
            , manualMode
            , residuesMode
            , autoMode
            , constellationResults
            , complete
            ]


tutorialWindow : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
tutorialWindow =
    styled Styled.div
        [ Css.backgroundColor Fancy.colourPalette.c2
        , Css.boxShadow5
            Css.zero
            Css.zero
            (Css.px 10)
            Css.zero
            (Css.rgba 100 100 100 1)
        , Css.maxHeight (Css.pct 100)
        , Css.overflow Css.auto
        , Css.padding (Css.px 5)
        ]



-- Welcome


welcome : Config msg -> Section msg
welcome { next, cancel } =
    { tutorialWindow =
        tutorialWindow [ css [ Css.width (Css.pct 80) ] ]
            [ Fancy.h2 [] [ text "Welcome!" ]
            , Fancy.p []
                [ "Hello and welcome to BUDE Alanine Scan. This tutorial will "
                    ++ "guide you through the basics of the application. "
                    |> text
                ]
            , Fancy.button [ onClick next ] [ text "Next" ]
            , Fancy.button [ onClick cancel ] [ text "Cancel" ]
            ]
    , model = Model.emptyModel
    , command = Ports.clearViewer ()
    }



-- Modes and overview


modes : Config msg -> Section msg
modes { previous, next, cancel } =
    { tutorialWindow =
        tutorialWindow
            [ css
                [ Css.width (Css.pct 80)
                , Css.maxHeight (Css.pct 80)
                ]
            ]
            [ Fancy.h2 [] [ text "Modes" ]
            , modeExplaination
            , Fancy.button [ onClick previous ] [ text "Previous" ]
            , Fancy.button [ onClick next ] [ text "Next" ]
            , Fancy.button [ onClick cancel ] [ text "Cancel" ]
            ]
    , model = Model.emptyModel
    , command = Cmd.none
    }


modeExplaination : Html msg
modeExplaination =
    """BUDE Alanine Scan (BAlaS) has two modes of operation: scan and
constellation. First we'll talk about scan then we'll come back to
constellation.

Scan performs standard computational alanine scan, where every residue
of the polypeptide of interest (called the _ligand_ in the app) is mutated to
alanine in turn. The interaction energy between each mutant of the _ligand_ and
another polypeptide (called the _receptor_ in the app) is calculated using the
[BUDE all-atom force
field](http://journals.sagepub.com/doi/abs/10.1177/1094342014528252). The
difference in the energy of the wild-type sequence _ligand_/_receptor_
interaction and the mutant _ligand_/_receptor_ interaction, known as the ΔΔG, is
calculated, which quantifies the energetic contribution of the residue to
_ligand_/_receptor_ interaction.
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


gettingStarted : Config msg -> Section msg
gettingStarted { previous, next, cancel } =
    { tutorialWindow =
        div
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.height (Css.pct 100)
                , Css.justifyContent Css.center
                , Css.left Css.zero
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.width (Css.pct 60)
                ]
            ]
            [ tutorialWindow
                [ css
                    [ Css.width (Css.pct 80)
                    ]
                ]
                [ Fancy.h2 [] [ text "Getting Started" ]
                , gettingStartedText
                , Fancy.button [ onClick previous ] [ text "Previous" ]
                , Fancy.button [ onClick next ] [ text "Next" ]
                , Fancy.button [ onClick cancel ] [ text "Cancel" ]
                ]
            ]
    , model = emptyModel
    , command = Ports.clearViewer ()
    }


gettingStartedText : Html msg
gettingStartedText =
    """The first thing you need to do to use BAlaS is to load in your structure
of interest (it currently needs to be in PDB format). To do this click the
"Choose File" button in the Scan panel and find the file on your computer.

> BAlaS doesn't work with all PDB files. It can't handle some badly formatted
> files or structures with non-canonical amino acids. If the job fails for this
> reason it should tell you in the notifications.
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


theViewer : Config msg -> Section msg
theViewer { previous, next, cancel } =
    { tutorialWindow =
        div
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.height (Css.pct 100)
                , Css.justifyContent Css.center
                , Css.left (Css.pct 60)
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.width (Css.pct 40)
                ]
            ]
            [ tutorialWindow
                [ css
                    [ Css.width (Css.pct 80)
                    ]
                ]
                [ Fancy.h2 [] [ text "The Molecular Viewer" ]
                , theViewerText
                , Fancy.button [ onClick previous ] [ text "Previous" ]
                , Fancy.button [ onClick next ] [ text "Next" ]
                , Fancy.button [ onClick cancel ] [ text "Cancel" ]
                ]
            ]
    , model =
        { emptyModel
            | alanineScan =
                { emptyAlaScanModel | structure = Just pdb1ycr }
        }
    , command = Ports.showStructure pdb1ycr.pdbFile
    }


theViewerText : Html msg
theViewerText =
    """Once your structure's loaded, it'll be visible in the viewer in the main
pane. The controls for the viewer are pretty simple:

- Left click and hold to rotate.
- Mouse wheel to zoom.
- Shift-left click to pan.
- Right click to save an image of your molecule.

If you click ⚛️  button at the top of the screen, you'll get some limited viewer
options, but you're better off doing that kind of thing in dedicated software
like [Chimera](https://www.cgl.ucsf.edu/chimera/) or
[Pymol](https://pymol.org/2/).
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


scanSubmission : Config msg -> Section msg
scanSubmission { previous, next, cancel } =
    { tutorialWindow =
        div
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.height (Css.pct 100)
                , Css.justifyContent Css.center
                , Css.left Css.zero
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.width (Css.pct 60)
                ]
            ]
            [ tutorialWindow
                [ css
                    [ Css.width (Css.pct 80)
                    ]
                ]
                [ Fancy.h2 [] [ text "Scan Submission" ]
                , scanSubmissionText
                , Fancy.button [ onClick previous ] [ text "Previous" ]
                , Fancy.button [ onClick next ] [ text "Next" ]
                , Fancy.button [ onClick cancel ] [ text "Cancel" ]
                ]
            ]
    , model =
        { emptyModel
            | alanineScan =
                { emptyAlaScanModel
                    | structure = Just pdb1ycr
                    , alanineScanSub =
                        AlanineScanSub
                            "1ycr AB Scan"
                            [ "A" ]
                            [ "B" ]
                            RemoteData.NotAsked
                }
        }
    , command = Cmd.none
    }


scanSubmissionText : Html msg
scanSubmissionText =
    """To submit a Scan job you need choose a name for the job, define the
_receptor_ region and define the _ligand_ region. Both the ligand and the
receptor can consist of one or more protein chains of the input structure.

> The _receptor_ and the _ligand_ are the regions of the protein that you
> want to measure the interaction energy between. The _ligand_ will be
> mutated during the computational alanine scan.

Once you've done all that, you can click "Start Scan" to send the job to the
server.
    """
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


theJobsPanel : Config msg -> Section msg
theJobsPanel { previous, next, cancel } =
    { tutorialWindow =
        div
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.height (Css.pct 100)
                , Css.justifyContent Css.center
                , Css.left Css.zero
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.width (Css.pct 60)
                ]
            ]
            [ tutorialWindow
                [ css
                    [ Css.width (Css.pct 80)
                    ]
                ]
                [ Fancy.h2 [] [ text "The Jobs Panel" ]
                , theJobsPanelText
                , Fancy.button [ onClick previous ] [ text "Previous" ]
                , Fancy.button [ onClick next ] [ text "Next" ]
                , Fancy.button [ onClick cancel ] [ text "Cancel" ]
                ]
            ]
    , model =
        { emptyModel
            | appMode = Model.Jobs
            , alanineScan =
                { emptyAlaScanModel
                    | jobs =
                        [ JobDetails
                            "5bb1eca7559d620012f74c31"
                            "1ycr AB Scan"
                            Model.Completed
                            Nothing
                        ]
                }
        }
    , command = Ports.clearViewer ()
    }


theJobsPanelText : Html msg
theJobsPanelText =
    """This panel shows all the jobs you've submitted to the server. Scan jobs
should finish quickly, but this depends on the size of the structure and the
queue on the server. You'll be notified when your job is finished in the
notification pane, which you can open with the 🔔 button.

Once the job is complete you can click "Show Results" to display the
results of the alanine scan. Your job list is persistent so you can come back to
the jobs panel and reload your results at any time, even after you close the
browser.

There are some other options too:

* *Copy Results Link to Clipboard* - This copies a link to the results of your
job to your clipboard. You can share this link with anyone.
* *Download Full Output* - Downloads a zip file containing the full output
produced by the commandline application. This even includes Chimera scripts for
displaying the results of the job.
* *Delete* - Deletes a job from your jobs list, but not on the server, so anyone
with a link can still see the output.
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


exampleScanResults : Model.AlanineScanResults
exampleScanResults =
    Model.AlanineScanResults
        "1ycr AB Scan"
        pdb1ycr.pdbFile
        [ "A" ]
        [ "B" ]
        -154.6038
        []
        [ Model.ResidueResult "17" "GLU" "B" 19.1207 0 0
        , Model.ResidueResult "18" "THR" "B" 0.5949 0 0
        , Model.ResidueResult "19" "PHE" "B" 20.3646 0 0
        , Model.ResidueResult "21" "ASP" "B" 0.0506 0 0
        , Model.ResidueResult "22" "LEU" "B" 6.7914 0 0
        , Model.ResidueResult "23" "TRP" "B" 22.0543 0 0
        , Model.ResidueResult "24" "LYS" "B" 0.5078 0 0
        , Model.ResidueResult "25" "LEU" "B" 0.4186 0 0
        , Model.ResidueResult "26" "LEU" "B" 8.3254 0 0
        , Model.ResidueResult "27" "PRO" "B" 4.1877 0 0
        , Model.ResidueResult "28" "GLU" "B" 15.6824 0 0
        , Model.ResidueResult "29" "ASN" "B" 0.5048 0 0
        ]


scanResults : Config msg -> Section msg
scanResults { previous, next, cancel } =
    { tutorialWindow =
        div
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.height (Css.pct 100)
                , Css.justifyContent Css.center
                , Css.left Css.zero
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.width (Css.pct 60)
                ]
            ]
            [ tutorialWindow
                [ css
                    [ Css.width (Css.pct 80)
                    ]
                ]
                [ Fancy.h2 [] [ text "Alanine Scan Results" ]
                , scanResultsText
                , Fancy.button [ onClick previous ] [ text "Previous" ]
                , Fancy.button [ onClick next ] [ text "Next" ]
                , Fancy.button [ onClick cancel ] [ text "Cancel" ]
                ]
            ]
    , model =
        { emptyModel
            | alanineScan =
                { emptyAlaScanModel
                    | results =
                        Just exampleScanResults
                    , structure = Just <| Model.Structure "" [] []
                }
        }
    , command = Ports.displayScanResults exampleScanResults
    }


scanResultsText : Html msg
scanResultsText =
    """Next you'll be taken back to the scan tab and the results will be
displayed. These include the ΔG of the interaction between the _ligand_ and the
_receptor_ and a table containing information about all the residues that have a
non-zero ΔΔG, as these residues form meaningful interactions with the
_receptor_. The more positive the ΔΔG value is for a residue, the larger a
contribution it makes to forming the interaction. If you hover over a residue in
the table, it will be highlighted on the structure.
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


constellationBasics : Config msg -> Section msg
constellationBasics { previous, next, cancel } =
    { tutorialWindow =
        div
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.height (Css.pct 100)
                , Css.justifyContent Css.center
                , Css.left Css.zero
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.width (Css.pct 60)
                ]
            ]
            [ tutorialWindow
                [ css
                    [ Css.width (Css.pct 80)
                    ]
                ]
                [ Fancy.h2 [] [ text "Constellation Jobs" ]
                , constellationBasicsText
                , Fancy.button [ onClick previous ] [ text "Previous" ]
                , Fancy.button [ onClick next ] [ text "Next" ]
                , Fancy.button [ onClick cancel ] [ text "Cancel" ]
                ]
            ]
    , model =
        { emptyModel
            | alanineScan =
                { emptyAlaScanModel
                    | results =
                        Just exampleScanResults
                    , structure = Just <| Model.Structure "" [] []
                }
            , appMode = Model.Constellation
        }
    , command = Ports.displayScanResults exampleScanResults
    }


constellationBasicsText : Html msg
constellationBasicsText =
    """Once you've got the results from a Scan job you can run a Constellation
job. In Constellation mode you can combine multiple alanine mutations and look
for clusters of _ligand_ residues that interact cooperatively with the
_receptor_ _i.e._ the sum of their individual ΔΔG values is less than the ΔΔG
when all residues are mutated to alanine simultaneously. This is useful for
highlighting regions of the _ligand_ that are key to forming the interaction
with the _receptor_.

There are three constellation job modes: "Manual", "Residues" and "Auto".
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


manualMode : Config msg -> Section msg
manualMode { previous, next, cancel } =
    { tutorialWindow =
        div
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.height (Css.pct 100)
                , Css.justifyContent Css.center
                , Css.left Css.zero
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.width (Css.pct 60)
                ]
            ]
            [ tutorialWindow
                [ css
                    [ Css.width (Css.pct 80)
                    ]
                ]
                [ Fancy.h2 [] [ text "Manual Mode" ]
                , manualModeText
                , Fancy.button [ onClick previous ] [ text "Previous" ]
                , Fancy.button [ onClick next ] [ text "Next" ]
                , Fancy.button [ onClick cancel ] [ text "Cancel" ]
                ]
            ]
    , model =
        { emptyModel
            | alanineScan =
                { emptyAlaScanModel
                    | results =
                        Just exampleScanResults
                    , structure = Just <| Model.Structure "" [] []
                }
            , constellation =
                { emptyConstellationModel
                    | constellationSub =
                        Model.Manual
                            { name = "1ycr PHE TRP LEU"
                            , residues =
                                [ "B19", "B23", "B26" ]
                                    |> Set.fromList
                            }
                }
            , appMode = Model.Constellation
        }
    , command = Ports.displayScanResults exampleScanResults
    }


manualModeText : Html msg
manualModeText =
    """In manual mode you select all the residues that you want to include in
the constellation. To run the job, give it a name and click residues in the
table to select them. Once you're done you can click "Submit".
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


residuesMode : Config msg -> Section msg
residuesMode { previous, next, cancel } =
    { tutorialWindow =
        div
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.height (Css.pct 100)
                , Css.justifyContent Css.center
                , Css.left Css.zero
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.width (Css.pct 60)
                ]
            ]
            [ tutorialWindow
                [ css
                    [ Css.width (Css.pct 80)
                    ]
                ]
                [ Fancy.h2 [] [ text "Residues Mode" ]
                , residuesModeText
                , Fancy.button [ onClick previous ] [ text "Previous" ]
                , Fancy.button [ onClick next ] [ text "Next" ]
                , Fancy.button [ onClick cancel ] [ text "Cancel" ]
                ]
            ]
    , model =
        { emptyModel
            | alanineScan =
                { emptyAlaScanModel
                    | results =
                        Just exampleScanResults
                    , structure = Just <| Model.Structure "" [] []
                }
            , constellation =
                { emptyConstellationModel
                    | constellationSub =
                        Model.Residues
                            { name = "1ycr Hydrophobic Face"
                            , constellationSize = 3
                            , residues =
                                [ "B19", "B23", "B26", "B27" ]
                                    |> Set.fromList
                            }
                }
            , appMode = Model.Constellation
        }
    , command = Ports.displayScanResults exampleScanResults
    }


residuesModeText : Html msg
residuesModeText =
    """In Residues mode you select any number of residues you want to use to
create constellations and set the constellation size. BAlaS will then
automatically create all combinations of residues for that constellation size.

> You cannot submit a job with less residues selected than the size of your
> constellation.
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


autoMode : Config msg -> Section msg
autoMode { previous, next, cancel } =
    { tutorialWindow =
        div
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.height (Css.pct 100)
                , Css.justifyContent Css.center
                , Css.left Css.zero
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.width (Css.pct 60)
                ]
            ]
            [ tutorialWindow
                [ css
                    [ Css.width (Css.pct 80)
                    ]
                ]
                [ Fancy.h2 [] [ text "Auto Mode" ]
                , autoModeText
                , Fancy.button [ onClick previous ] [ text "Previous" ]
                , Fancy.button [ onClick next ] [ text "Next" ]
                , Fancy.button [ onClick cancel ] [ text "Cancel" ]
                ]
            ]
    , model =
        { emptyModel
            | alanineScan =
                { emptyAlaScanModel
                    | results =
                        Just exampleScanResults
                    , structure = Just <| Model.Structure "" [] []
                }
            , constellation =
                { emptyConstellationModel
                    | constellationSub =
                        Model.Auto
                            { name = "1ycr ddg5 con3 co15"
                            , ddGCutOff = "15.0"
                            , constellationSize = "3"
                            , cutOffDistance = "15.0"
                            }
                }
            , appMode = Model.Constellation
        }
    , command = Ports.displayScanResults exampleScanResults
    }


autoModeText : Html msg
autoModeText =
    """In auto mode you select a ΔΔG cut off value and all residues above that
value will be used to create constellations, so long as the residues are within
the distance cut off of each other. The residues that meet the ΔΔG cut off are
shown in a list under the submit button.

> Make sure that you have enough residues that meet your criteria to make your
> desired size of constellation.
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


constellationResults : Config msg -> Section msg
constellationResults { previous, next, cancel } =
    let
        results =
            { name = "1ycr Hydrophobic Face"
            , hotConstellations =
                [ ( "B19_B22_B23", -107.269 )
                , ( "B19_B22_B26", -120.996 )
                , ( "B19_B23_B26", -103.846 )
                , ( "B22_B23_B26", -119.308 )
                ]
            , scanResults = exampleScanResults
            }
    in
    { tutorialWindow =
        div
            [ css
                [ Css.alignItems Css.center
                , Css.displayFlex
                , Css.height (Css.pct 100)
                , Css.justifyContent Css.center
                , Css.left Css.zero
                , Css.position Css.absolute
                , Css.top Css.zero
                , Css.width (Css.pct 60)
                ]
            ]
            [ tutorialWindow
                [ css
                    [ Css.width (Css.pct 80)
                    ]
                ]
                [ Fancy.h2 [] [ text "Constellation Results" ]
                , constellationResultsText
                , Fancy.button [ onClick previous ] [ text "Previous" ]
                , Fancy.button [ onClick next ] [ text "Next" ]
                , Fancy.button [ onClick cancel ] [ text "Cancel" ]
                ]
            ]
    , model =
        { emptyModel
            | alanineScan =
                { emptyAlaScanModel
                    | results =
                        Just exampleScanResults
                    , structure = Just <| Model.Structure "" [] []
                }
            , constellation =
                { emptyConstellationModel
                    | results =
                        Just results
                }
            , appMode = Model.Constellation
        }
    , command = Cmd.none
    }


constellationResultsText : Html msg
constellationResultsText =
    """Once your constellation job is finished you can get the results from the
Job tab. The Constellation pane will then display a table with information about
each constellation describing the residues involved, the interaction energy
between the constellation and the _receptor_ and the summed energies of the
individual residues in the constellation. The cooperativity of the constellation
can be determined by comparing the summed individual energies with the
interaction energy of the constellation.
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled


complete : Config msg -> Section msg
complete { previous, cancel } =
    { tutorialWindow =
        tutorialWindow [ css [ Css.width (Css.pct 80) ] ]
            [ Fancy.h2 [] [ text "Tutorial Complete!" ]
            , completeText
            , Fancy.button [ onClick previous ] [ text "Previous" ]
            , Fancy.button [ onClick cancel ] [ text "Finish" ]
            ]
    , model = Model.emptyModel
    , command = Ports.clearViewer ()
    }


completeText : Html msg
completeText =
    """You've now completed the BUDE Alanine Scan tutorial. You can revisit it
anytime by clicking the "❓" button. This web app provides a simple web
interface to the much more powerful BAlaS commandline app. If you want to scale
up your analysis you can download the commandline app and run it on your own
computer. Thanks for using BAlaS!
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled
