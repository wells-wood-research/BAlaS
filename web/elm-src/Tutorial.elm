module Tutorial exposing
    ( Config
    , Section
    , Tutorial(..)
    , moveBackwards
    , moveForward
    , tutorial
    )

import Css
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
    , command = Ports.showStructure pdb1ycrString
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


pdb1ycrString : String
pdb1ycrString =
    """HEADER    COMPLEX (ONCOGENE PROTEIN/PEPTIDE)      30-SEP-96   1YCR              
TITLE     MDM2 BOUND TO THE TRANSACTIVATION DOMAIN OF P53                       
HELIX    1   1 PRO A   32  VAL A   41  1                                  10    
HELIX    2   2 MET A   50  THR A   63  1                                  14    
HELIX    3   3 LEU A   81  PHE A   86  1                                   6    
HELIX    4   4 HIS A   96  ASN A  106  1                                  11    
HELIX    5   5 PHE B   19  LEU B   25  1                                   7    
SHEET    1   A 2 ILE A  74  TYR A  76  0                                        
SHEET    2   A 2 SER A  90  SER A  92 -1  N  PHE A  91   O  VAL A  75           
CRYST1   43.414  100.546   54.853  90.00  90.00  90.00 C 2 2 21      8          
ORIGX1      1.000000  0.000000  0.000000        0.00000                         
ORIGX2      0.000000  1.000000  0.000000        0.00000                         
ORIGX3      0.000000  0.000000  1.000000        0.00000                         
SCALE1      0.023034  0.000000  0.000000        0.00000                         
SCALE2      0.000000  0.009946  0.000000        0.00000                         
SCALE3      0.000000  0.000000  0.018231        0.00000                         
ATOM      1  N   GLU A  25      10.801 -12.147  -5.180  1.00 49.08           N  
ATOM      2  CA  GLU A  25      11.124 -13.382  -4.414  1.00 50.43           C  
ATOM      3  C   GLU A  25      11.769 -12.878  -3.130  1.00 49.75           C  
ATOM      4  O   GLU A  25      11.175 -12.047  -2.441  1.00 49.34           O  
ATOM      5  CB  GLU A  25      12.075 -14.259  -5.228  1.00 53.69           C  
ATOM      6  CG  GLU A  25      11.564 -14.543  -6.638  1.00 57.47           C  
ATOM      7  CD  GLU A  25      12.414 -15.556  -7.397  1.00 61.03           C  
ATOM      8  OE1 GLU A  25      12.439 -16.745  -6.995  1.00 61.91           O  
ATOM      9  OE2 GLU A  25      13.043 -15.163  -8.409  1.00 62.06           O  
ATOM     10  N   THR A  26      12.965 -13.355  -2.800  1.00 49.81           N  
ATOM     11  CA  THR A  26      13.656 -12.864  -1.611  1.00 48.43           C  
ATOM     12  C   THR A  26      14.518 -11.718  -2.102  1.00 44.63           C  
ATOM     13  O   THR A  26      15.519 -11.956  -2.778  1.00 45.84           O  
ATOM     14  CB  THR A  26      14.598 -13.915  -1.009  1.00 49.13           C  
ATOM     15  OG1 THR A  26      15.478 -14.396  -2.032  1.00 49.45           O  
ATOM     16  CG2 THR A  26      13.804 -15.068  -0.397  1.00 49.70           C  
ATOM     17  N   LEU A  27      14.081 -10.486  -1.869  1.00 38.63           N  
ATOM     18  CA  LEU A  27      14.863  -9.337  -2.294  1.00 35.16           C  
ATOM     19  C   LEU A  27      15.877  -8.978  -1.213  1.00 32.59           C  
ATOM     20  O   LEU A  27      15.495  -8.696  -0.082  1.00 30.66           O  
ATOM     21  CB  LEU A  27      13.960  -8.146  -2.616  1.00 32.87           C  
ATOM     22  CG  LEU A  27      13.336  -8.100  -4.010  1.00 29.89           C  
ATOM     23  CD1 LEU A  27      13.942  -6.978  -4.794  1.00 29.32           C  
ATOM     24  CD2 LEU A  27      13.546  -9.397  -4.744  1.00 27.86           C  
ATOM     25  N   VAL A  28      17.162  -9.063  -1.553  1.00 30.41           N  
ATOM     26  CA  VAL A  28      18.243  -8.753  -0.623  1.00 26.97           C  
ATOM     27  C   VAL A  28      19.056  -7.585  -1.146  1.00 29.59           C  
ATOM     28  O   VAL A  28      18.977  -7.247  -2.329  1.00 29.91           O  
ATOM     29  CB  VAL A  28      19.188  -9.980  -0.384  1.00 20.37           C  
ATOM     30  CG1 VAL A  28      18.428 -11.110   0.261  1.00 17.22           C  
ATOM     31  CG2 VAL A  28      19.819 -10.441  -1.688  1.00 19.17           C  
ATOM     32  N   ARG A  29      19.785  -6.941  -0.241  1.00 33.20           N  
ATOM     33  CA  ARG A  29      20.650  -5.812  -0.577  1.00 37.19           C  
ATOM     34  C   ARG A  29      22.031  -6.257  -0.132  1.00 34.37           C  
ATOM     35  O   ARG A  29      22.238  -6.530   1.050  1.00 35.05           O  
ATOM     36  CB  ARG A  29      20.239  -4.577   0.213  1.00 46.45           C  
ATOM     37  CG  ARG A  29      20.885  -3.295  -0.264  1.00 56.87           C  
ATOM     38  CD  ARG A  29      20.354  -2.071   0.489  1.00 62.95           C  
ATOM     39  NE  ARG A  29      18.892  -1.904   0.445  1.00 67.51           N  
ATOM     40  CZ  ARG A  29      18.168  -1.674  -0.657  1.00 69.80           C  
ATOM     41  NH1 ARG A  29      18.743  -1.578  -1.853  1.00 70.00           N  
ATOM     42  NH2 ARG A  29      16.853  -1.509  -0.560  1.00 70.00           N  
ATOM     43  N   PRO A  30      22.976  -6.407  -1.075  1.00 32.61           N  
ATOM     44  CA  PRO A  30      24.328  -6.842  -0.726  1.00 31.66           C  
ATOM     45  C   PRO A  30      25.179  -5.801   0.008  1.00 31.16           C  
ATOM     46  O   PRO A  30      25.136  -4.598  -0.297  1.00 33.84           O  
ATOM     47  CB  PRO A  30      24.915  -7.215  -2.086  1.00 31.15           C  
ATOM     48  CG  PRO A  30      24.291  -6.227  -2.989  1.00 31.13           C  
ATOM     49  CD  PRO A  30      22.853  -6.222  -2.532  1.00 32.34           C  
ATOM     50  N   LYS A  31      25.928  -6.279   0.997  1.00 26.84           N  
ATOM     51  CA  LYS A  31      26.812  -5.440   1.782  1.00 25.65           C  
ATOM     52  C   LYS A  31      27.962  -4.950   0.897  1.00 25.77           C  
ATOM     53  O   LYS A  31      28.271  -5.569  -0.123  1.00 24.89           O  
ATOM     54  CB  LYS A  31      27.298  -6.203   3.022  1.00 27.30           C  
ATOM     55  CG  LYS A  31      26.247  -6.219   4.140  1.00 28.71           C  
ATOM     56  CD  LYS A  31      26.650  -7.040   5.370  1.00 31.15           C  
ATOM     57  CE  LYS A  31      25.543  -6.988   6.439  1.00 31.90           C  
ATOM     58  NZ  LYS A  31      25.561  -8.096   7.463  1.00 32.60           N  
ATOM     59  N   PRO A  32      28.632  -3.852   1.294  1.00 27.32           N  
ATOM     60  CA  PRO A  32      29.752  -3.210   0.585  1.00 28.83           C  
ATOM     61  C   PRO A  32      30.758  -4.109  -0.155  1.00 28.49           C  
ATOM     62  O   PRO A  32      30.962  -3.960  -1.368  1.00 26.97           O  
ATOM     63  CB  PRO A  32      30.415  -2.397   1.693  1.00 30.47           C  
ATOM     64  CG  PRO A  32      29.249  -1.972   2.513  1.00 29.36           C  
ATOM     65  CD  PRO A  32      28.486  -3.257   2.637  1.00 27.44           C  
ATOM     66  N   LEU A  33      31.405  -5.018   0.569  1.00 28.99           N  
ATOM     67  CA  LEU A  33      32.370  -5.908  -0.059  1.00 30.75           C  
ATOM     68  C   LEU A  33      31.709  -6.751  -1.160  1.00 29.16           C  
ATOM     69  O   LEU A  33      32.216  -6.834  -2.292  1.00 28.31           O  
ATOM     70  CB  LEU A  33      33.014  -6.811   1.000  1.00 34.41           C  
ATOM     71  CG  LEU A  33      34.513  -6.663   1.322  1.00 36.11           C  
ATOM     72  CD1 LEU A  33      34.816  -5.269   1.837  1.00 35.52           C  
ATOM     73  CD2 LEU A  33      34.923  -7.714   2.356  1.00 36.44           C  
ATOM     74  N   LEU A  34      30.544  -7.316  -0.838  1.00 26.60           N  
ATOM     75  CA  LEU A  34      29.801  -8.160  -1.765  1.00 24.24           C  
ATOM     76  C   LEU A  34      29.264  -7.399  -2.961  1.00 23.47           C  
ATOM     77  O   LEU A  34      29.189  -7.933  -4.068  1.00 24.63           O  
ATOM     78  CB  LEU A  34      28.655  -8.872  -1.044  1.00 23.85           C  
ATOM     79  CG  LEU A  34      27.797  -9.797  -1.918  1.00 22.79           C  
ATOM     80  CD1 LEU A  34      28.677 -10.890  -2.495  1.00 20.28           C  
ATOM     81  CD2 LEU A  34      26.620 -10.369  -1.110  1.00 22.38           C  
ATOM     82  N   LEU A  35      28.892  -6.149  -2.747  1.00 22.96           N  
ATOM     83  CA  LEU A  35      28.366  -5.336  -3.833  1.00 23.98           C  
ATOM     84  C   LEU A  35      29.470  -5.044  -4.855  1.00 26.54           C  
ATOM     85  O   LEU A  35      29.231  -5.039  -6.069  1.00 27.25           O  
ATOM     86  CB  LEU A  35      27.784  -4.054  -3.268  1.00 22.75           C  
ATOM     87  CG  LEU A  35      26.684  -3.377  -4.074  1.00 23.66           C  
ATOM     88  CD1 LEU A  35      27.195  -2.000  -4.438  1.00 26.56           C  
ATOM     89  CD2 LEU A  35      26.257  -4.189  -5.304  1.00 21.58           C  
ATOM     90  N   LYS A  36      30.678  -4.830  -4.340  1.00 28.69           N  
ATOM     91  CA  LYS A  36      31.883  -4.570  -5.132  1.00 31.72           C  
ATOM     92  C   LYS A  36      32.209  -5.825  -5.981  1.00 31.71           C  
ATOM     93  O   LYS A  36      32.513  -5.730  -7.173  1.00 33.69           O  
ATOM     94  CB  LYS A  36      33.029  -4.269  -4.166  1.00 35.41           C  
ATOM     95  CG  LYS A  36      34.359  -4.005  -4.810  1.00 39.49           C  
ATOM     96  CD  LYS A  36      35.454  -4.022  -3.755  1.00 43.89           C  
ATOM     97  CE  LYS A  36      35.130  -3.090  -2.589  1.00 46.95           C  
ATOM     98  NZ  LYS A  36      36.283  -2.868  -1.655  1.00 47.38           N  
ATOM     99  N   LEU A  37      32.162  -6.990  -5.339  1.00 27.34           N  
ATOM    100  CA  LEU A  37      32.387  -8.279  -5.983  1.00 24.68           C  
ATOM    101  C   LEU A  37      31.378  -8.413  -7.125  1.00 25.88           C  
ATOM    102  O   LEU A  37      31.711  -8.866  -8.216  1.00 23.86           O  
ATOM    103  CB  LEU A  37      32.113  -9.371  -4.943  1.00 25.16           C  
ATOM    104  CG  LEU A  37      32.290 -10.884  -5.107  1.00 26.21           C  
ATOM    105  CD1 LEU A  37      31.802 -11.369  -6.456  1.00 27.27           C  
ATOM    106  CD2 LEU A  37      33.737 -11.232  -4.901  1.00 26.79           C  
ATOM    107  N   LEU A  38      30.130  -8.030  -6.846  1.00 30.28           N  
ATOM    108  CA  LEU A  38      29.030  -8.111  -7.813  1.00 29.47           C  
ATOM    109  C   LEU A  38      29.248  -7.215  -9.012  1.00 31.49           C  
ATOM    110  O   LEU A  38      29.178  -7.680 -10.151  1.00 31.19           O  
ATOM    111  CB  LEU A  38      27.696  -7.777  -7.134  1.00 24.98           C  
ATOM    112  CG  LEU A  38      26.681  -8.896  -6.865  1.00 20.36           C  
ATOM    113  CD1 LEU A  38      27.327 -10.238  -6.748  1.00 17.92           C  
ATOM    114  CD2 LEU A  38      25.941  -8.569  -5.596  1.00 20.85           C  
ATOM    115  N   LYS A  39      29.521  -5.934  -8.756  1.00 33.42           N  
ATOM    116  CA  LYS A  39      29.764  -4.979  -9.833  1.00 34.82           C  
ATOM    117  C   LYS A  39      30.869  -5.531 -10.710  1.00 35.75           C  
ATOM    118  O   LYS A  39      30.754  -5.535 -11.939  1.00 37.32           O  
ATOM    119  CB  LYS A  39      30.171  -3.603  -9.293  1.00 38.06           C  
ATOM    120  CG  LYS A  39      29.133  -2.935  -8.392  1.00 41.92           C  
ATOM    121  CD  LYS A  39      27.729  -3.178  -8.919  1.00 44.44           C  
ATOM    122  CE  LYS A  39      26.630  -2.584  -8.055  1.00 46.35           C  
ATOM    123  NZ  LYS A  39      26.391  -1.133  -8.324  1.00 46.80           N  
ATOM    124  N   SER A  40      31.889  -6.096 -10.069  1.00 36.07           N  
ATOM    125  CA  SER A  40      33.022  -6.663 -10.783  1.00 36.72           C  
ATOM    126  C   SER A  40      32.586  -7.569 -11.941  1.00 37.40           C  
ATOM    127  O   SER A  40      33.299  -7.675 -12.944  1.00 39.94           O  
ATOM    128  CB  SER A  40      33.939  -7.422  -9.816  1.00 37.52           C  
ATOM    129  OG  SER A  40      33.633  -8.802  -9.782  1.00 39.51           O  
ATOM    130  N   VAL A  41      31.416  -8.202 -11.824  1.00 34.16           N  
ATOM    131  CA  VAL A  41      30.957  -9.077 -12.896  1.00 30.18           C  
ATOM    132  C   VAL A  41      29.807  -8.535 -13.711  1.00 29.68           C  
ATOM    133  O   VAL A  41      29.119  -9.299 -14.378  1.00 32.08           O  
ATOM    134  CB  VAL A  41      30.651 -10.512 -12.411  1.00 26.76           C  
ATOM    135  CG1 VAL A  41      31.934 -11.220 -12.068  1.00 26.46           C  
ATOM    136  CG2 VAL A  41      29.764 -10.488 -11.204  1.00 25.44           C  
ATOM    137  N   GLY A  42      29.618  -7.220 -13.684  1.00 27.40           N  
ATOM    138  CA  GLY A  42      28.554  -6.605 -14.461  1.00 26.98           C  
ATOM    139  C   GLY A  42      27.242  -6.280 -13.771  1.00 29.87           C  
ATOM    140  O   GLY A  42      26.435  -5.515 -14.299  1.00 31.25           O  
ATOM    141  N   ALA A  43      27.006  -6.857 -12.599  1.00 31.86           N  
ATOM    142  CA  ALA A  43      25.767  -6.605 -11.863  1.00 33.11           C  
ATOM    143  C   ALA A  43      25.743  -5.173 -11.340  1.00 36.97           C  
ATOM    144  O   ALA A  43      26.183  -4.912 -10.224  1.00 37.99           O  
ATOM    145  CB  ALA A  43      25.629  -7.590 -10.709  1.00 31.88           C  
ATOM    146  N   GLN A  44      25.196  -4.258 -12.134  1.00 38.43           N  
ATOM    147  CA  GLN A  44      25.131  -2.841 -11.765  1.00 40.64           C  
ATOM    148  C   GLN A  44      23.950  -2.382 -10.881  1.00 38.73           C  
ATOM    149  O   GLN A  44      23.270  -1.417 -11.218  1.00 39.71           O  
ATOM    150  CB  GLN A  44      25.100  -1.997 -13.038  1.00 44.75           C  
ATOM    151  CG  GLN A  44      26.061  -2.371 -14.123  1.00 47.84           C  
ATOM    152  CD  GLN A  44      25.898  -1.446 -15.308  1.00 52.18           C  
ATOM    153  OE1 GLN A  44      24.827  -1.385 -15.918  1.00 53.63           O  
ATOM    154  NE2 GLN A  44      26.945  -0.680 -15.616  1.00 54.69           N  
ATOM    155  N   LYS A  45      23.712  -3.000  -9.735  1.00 36.33           N  
ATOM    156  CA  LYS A  45      22.587  -2.545  -8.939  1.00 35.35           C  
ATOM    157  C   LYS A  45      22.679  -2.802  -7.445  1.00 38.33           C  
ATOM    158  O   LYS A  45      23.568  -3.507  -6.978  1.00 40.86           O  
ATOM    159  CB  LYS A  45      21.277  -3.065  -9.526  1.00 34.15           C  
ATOM    160  CG  LYS A  45      21.215  -4.557  -9.704  1.00 33.47           C  
ATOM    161  CD  LYS A  45      19.840  -4.968 -10.170  1.00 33.07           C  
ATOM    162  CE  LYS A  45      19.581  -6.418  -9.848  1.00 33.29           C  
ATOM    163  NZ  LYS A  45      18.155  -6.740 -10.064  1.00 33.65           N  
ATOM    164  N   ASP A  46      21.731  -2.247  -6.703  1.00 39.51           N  
ATOM    165  CA  ASP A  46      21.717  -2.334  -5.253  1.00 43.18           C  
ATOM    166  C   ASP A  46      20.770  -3.333  -4.628  1.00 39.04           C  
ATOM    167  O   ASP A  46      20.897  -3.623  -3.454  1.00 39.67           O  
ATOM    168  CB  ASP A  46      21.387  -0.944  -4.706  1.00 53.36           C  
ATOM    169  CG  ASP A  46      21.760  -0.766  -3.242  1.00 63.20           C  
ATOM    170  OD1 ASP A  46      22.354  -1.691  -2.634  1.00 67.17           O  
ATOM    171  OD2 ASP A  46      21.475   0.336  -2.709  1.00 66.33           O  
ATOM    172  N   THR A  47      19.798  -3.829  -5.379  1.00 37.84           N  
ATOM    173  CA  THR A  47      18.826  -4.769  -4.826  1.00 37.77           C  
ATOM    174  C   THR A  47      18.751  -5.986  -5.729  1.00 33.96           C  
ATOM    175  O   THR A  47      18.709  -5.842  -6.947  1.00 34.85           O  
ATOM    176  CB  THR A  47      17.438  -4.093  -4.678  1.00 41.52           C  
ATOM    177  OG1 THR A  47      17.606  -2.806  -4.059  1.00 44.57           O  
ATOM    178  CG2 THR A  47      16.533  -4.918  -3.789  1.00 41.07           C  
ATOM    179  N   TYR A  48      18.757  -7.181  -5.139  1.00 29.39           N  
ATOM    180  CA  TYR A  48      18.751  -8.419  -5.918  1.00 25.54           C  
ATOM    181  C   TYR A  48      17.912  -9.518  -5.303  1.00 23.68           C  
ATOM    182  O   TYR A  48      17.644  -9.512  -4.108  1.00 24.17           O  
ATOM    183  CB  TYR A  48      20.180  -9.004  -5.990  1.00 23.33           C  
ATOM    184  CG  TYR A  48      21.250  -8.109  -6.562  1.00 20.34           C  
ATOM    185  CD1 TYR A  48      21.808  -7.089  -5.796  1.00 19.97           C  
ATOM    186  CD2 TYR A  48      21.694  -8.265  -7.875  1.00 19.16           C  
ATOM    187  CE1 TYR A  48      22.770  -6.248  -6.323  1.00 18.70           C  
ATOM    188  CE2 TYR A  48      22.656  -7.427  -8.400  1.00 19.06           C  
ATOM    189  CZ  TYR A  48      23.181  -6.424  -7.614  1.00 18.73           C  
ATOM    190  OH  TYR A  48      24.118  -5.576  -8.118  1.00 21.84           O  
ATOM    191  N   THR A  49      17.521 -10.484  -6.117  1.00 21.72           N  
ATOM    192  CA  THR A  49      16.819 -11.630  -5.577  1.00 22.96           C  
ATOM    193  C   THR A  49      17.970 -12.568  -5.189  1.00 23.47           C  
ATOM    194  O   THR A  49      19.105 -12.333  -5.602  1.00 21.98           O  
ATOM    195  CB  THR A  49      15.898 -12.290  -6.628  1.00 24.40           C  
ATOM    196  OG1 THR A  49      16.624 -12.552  -7.835  1.00 25.85           O  
ATOM    197  CG2 THR A  49      14.738 -11.367  -6.951  1.00 24.83           C  
ATOM    198  N   MET A  50      17.724 -13.577  -4.359  1.00 26.64           N  
ATOM    199  CA  MET A  50      18.794 -14.502  -3.998  1.00 28.05           C  
ATOM    200  C   MET A  50      19.413 -15.149  -5.246  1.00 28.41           C  
ATOM    201  O   MET A  50      20.624 -15.353  -5.303  1.00 31.46           O  
ATOM    202  CB  MET A  50      18.290 -15.585  -3.043  1.00 29.52           C  
ATOM    203  CG  MET A  50      18.786 -15.430  -1.616  1.00 31.67           C  
ATOM    204  SD  MET A  50      20.561 -15.673  -1.478  1.00 33.21           S  
ATOM    205  CE  MET A  50      20.698 -17.337  -2.115  1.00 33.22           C  
ATOM    206  N   LYS A  51      18.595 -15.443  -6.254  1.00 25.59           N  
ATOM    207  CA  LYS A  51      19.082 -16.054  -7.492  1.00 24.33           C  
ATOM    208  C   LYS A  51      20.123 -15.240  -8.275  1.00 20.13           C  
ATOM    209  O   LYS A  51      21.077 -15.814  -8.783  1.00 20.41           O  
ATOM    210  CB  LYS A  51      17.911 -16.421  -8.405  1.00 26.76           C  
ATOM    211  CG  LYS A  51      17.019 -17.491  -7.844  1.00 29.23           C  
ATOM    212  CD  LYS A  51      15.826 -17.759  -8.757  1.00 32.33           C  
ATOM    213  CE  LYS A  51      14.860 -18.758  -8.123  1.00 35.65           C  
ATOM    214  NZ  LYS A  51      13.726 -19.097  -9.045  1.00 39.61           N  
ATOM    215  N   GLU A  52      19.929 -13.927  -8.396  1.00 18.05           N  
ATOM    216  CA  GLU A  52      20.876 -13.065  -9.111  1.00 17.63           C  
ATOM    217  C   GLU A  52      22.214 -13.071  -8.387  1.00 18.96           C  
ATOM    218  O   GLU A  52      23.271 -13.141  -9.011  1.00 21.98           O  
ATOM    219  CB  GLU A  52      20.379 -11.623  -9.192  1.00 15.17           C  
ATOM    220  CG  GLU A  52      19.197 -11.420 -10.100  1.00 14.49           C  
ATOM    221  CD  GLU A  52      18.761  -9.957 -10.221  1.00 14.76           C  
ATOM    222  OE1 GLU A  52      18.401  -9.331  -9.195  1.00 14.10           O  
ATOM    223  OE2 GLU A  52      18.773  -9.430 -11.359  1.00 16.08           O  
ATOM    224  N   VAL A  53      22.162 -12.984  -7.063  1.00 16.80           N  
ATOM    225  CA  VAL A  53      23.367 -12.993  -6.256  1.00 15.61           C  
ATOM    226  C   VAL A  53      24.177 -14.251  -6.596  1.00 19.06           C  
ATOM    227  O   VAL A  53      25.280 -14.165  -7.121  1.00 19.53           O  
ATOM    228  CB  VAL A  53      23.022 -12.909  -4.732  1.00 10.83           C  
ATOM    229  CG1 VAL A  53      24.221 -13.377  -3.855  1.00  9.30           C  
ATOM    230  CG2 VAL A  53      22.617 -11.464  -4.356  1.00  6.78           C  
ATOM    231  N   LEU A  54      23.569 -15.414  -6.415  1.00 20.11           N  
ATOM    232  CA  LEU A  54      24.244 -16.672  -6.687  1.00 17.78           C  
ATOM    233  C   LEU A  54      24.751 -16.780  -8.105  1.00 18.36           C  
ATOM    234  O   LEU A  54      25.816 -17.334  -8.323  1.00 23.49           O  
ATOM    235  CB  LEU A  54      23.318 -17.850  -6.420  1.00 16.46           C  
ATOM    236  CG  LEU A  54      22.858 -18.137  -4.996  1.00 14.65           C  
ATOM    237  CD1 LEU A  54      21.786 -19.217  -5.048  1.00 15.10           C  
ATOM    238  CD2 LEU A  54      24.023 -18.574  -4.154  1.00 11.49           C  
ATOM    239  N   PHE A  55      23.983 -16.277  -9.065  1.00 15.68           N  
ATOM    240  CA  PHE A  55      24.341 -16.344 -10.489  1.00 12.20           C  
ATOM    241  C   PHE A  55      25.636 -15.630 -10.765  1.00 15.52           C  
ATOM    242  O   PHE A  55      26.546 -16.223 -11.342  1.00 17.40           O  
ATOM    243  CB  PHE A  55      23.223 -15.735 -11.350  1.00  6.10           C  
ATOM    244  CG  PHE A  55      23.615 -15.454 -12.776  1.00  1.58           C  
ATOM    245  CD1 PHE A  55      23.765 -16.477 -13.688  1.00  1.07           C  
ATOM    246  CD2 PHE A  55      23.835 -14.171 -13.197  1.00  1.00           C  
ATOM    247  CE1 PHE A  55      24.135 -16.221 -14.999  1.00  1.00           C  
ATOM    248  CE2 PHE A  55      24.200 -13.901 -14.497  1.00  1.00           C  
ATOM    249  CZ  PHE A  55      24.351 -14.915 -15.392  1.00  1.77           C  
ATOM    250  N   TYR A  56      25.697 -14.353 -10.376  1.00 18.18           N  
ATOM    251  CA  TYR A  56      26.886 -13.515 -10.570  1.00 21.08           C  
ATOM    252  C   TYR A  56      28.058 -14.009  -9.713  1.00 25.58           C  
ATOM    253  O   TYR A  56      29.203 -14.065 -10.171  1.00 27.28           O  
ATOM    254  CB  TYR A  56      26.588 -12.060 -10.211  1.00 19.30           C  
ATOM    255  CG  TYR A  56      25.665 -11.344 -11.164  1.00 18.72           C  
ATOM    256  CD1 TYR A  56      26.036 -11.122 -12.493  1.00 19.77           C  
ATOM    257  CD2 TYR A  56      24.418 -10.890 -10.739  1.00 17.69           C  
ATOM    258  CE1 TYR A  56      25.187 -10.464 -13.377  1.00 19.76           C  
ATOM    259  CE2 TYR A  56      23.566 -10.237 -11.602  1.00 18.54           C  
ATOM    260  CZ  TYR A  56      23.948 -10.022 -12.923  1.00 21.12           C  
ATOM    261  OH  TYR A  56      23.089  -9.339 -13.769  1.00 24.19           O  
ATOM    262  N   LEU A  57      27.760 -14.367  -8.468  1.00 25.21           N  
ATOM    263  CA  LEU A  57      28.758 -14.877  -7.542  1.00 21.43           C  
ATOM    264  C   LEU A  57      29.396 -16.144  -8.132  1.00 16.56           C  
ATOM    265  O   LEU A  57      30.583 -16.399  -7.952  1.00 16.93           O  
ATOM    266  CB  LEU A  57      28.097 -15.141  -6.188  1.00 21.93           C  
ATOM    267  CG  LEU A  57      28.991 -15.465  -4.993  1.00 23.25           C  
ATOM    268  CD1 LEU A  57      30.098 -14.433  -4.883  1.00 24.87           C  
ATOM    269  CD2 LEU A  57      28.163 -15.504  -3.729  1.00 22.07           C  
ATOM    270  N   GLY A  58      28.606 -16.903  -8.877  1.00 13.03           N  
ATOM    271  CA  GLY A  58      29.093 -18.105  -9.525  1.00 12.30           C  
ATOM    272  C   GLY A  58      29.881 -17.738 -10.769  1.00 15.61           C  
ATOM    273  O   GLY A  58      30.833 -18.418 -11.126  1.00 19.99           O  
ATOM    274  N   GLN A  59      29.489 -16.659 -11.427  1.00 14.86           N  
ATOM    275  CA  GLN A  59      30.166 -16.170 -12.622  1.00 16.02           C  
ATOM    276  C   GLN A  59      31.564 -15.657 -12.285  1.00 17.15           C  
ATOM    277  O   GLN A  59      32.486 -15.736 -13.091  1.00 20.04           O  
ATOM    278  CB  GLN A  59      29.368 -15.021 -13.216  1.00 18.21           C  
ATOM    279  CG  GLN A  59      28.089 -15.453 -13.826  1.00 21.13           C  
ATOM    280  CD  GLN A  59      28.336 -16.409 -14.942  1.00 23.42           C  
ATOM    281  OE1 GLN A  59      28.794 -16.018 -16.008  1.00 26.16           O  
ATOM    282  NE2 GLN A  59      28.071 -17.677 -14.700  1.00 24.91           N  
ATOM    283  N   TYR A  60      31.688 -15.061 -11.114  1.00 15.53           N  
ATOM    284  CA  TYR A  60      32.947 -14.515 -10.639  1.00 14.57           C  
ATOM    285  C   TYR A  60      33.933 -15.681 -10.434  1.00 16.46           C  
ATOM    286  O   TYR A  60      35.049 -15.679 -10.946  1.00 18.96           O  
ATOM    287  CB  TYR A  60      32.660 -13.784  -9.333  1.00 14.30           C  
ATOM    288  CG  TYR A  60      33.858 -13.295  -8.591  1.00 15.69           C  
ATOM    289  CD1 TYR A  60      34.378 -12.026  -8.831  1.00 15.89           C  
ATOM    290  CD2 TYR A  60      34.444 -14.077  -7.591  1.00 16.18           C  
ATOM    291  CE1 TYR A  60      35.444 -11.550  -8.088  1.00 15.25           C  
ATOM    292  CE2 TYR A  60      35.505 -13.607  -6.848  1.00 15.93           C  
ATOM    293  CZ  TYR A  60      35.996 -12.344  -7.101  1.00 16.11           C  
ATOM    294  OH  TYR A  60      37.017 -11.854  -6.336  1.00 18.25           O  
ATOM    295  N   ILE A  61      33.478 -16.702  -9.727  1.00 15.26           N  
ATOM    296  CA  ILE A  61      34.274 -17.877  -9.450  1.00 13.48           C  
ATOM    297  C   ILE A  61      34.684 -18.570 -10.724  1.00 16.31           C  
ATOM    298  O   ILE A  61      35.862 -18.831 -10.943  1.00 18.13           O  
ATOM    299  CB  ILE A  61      33.497 -18.858  -8.544  1.00 12.76           C  
ATOM    300  CG1 ILE A  61      33.109 -18.151  -7.243  1.00 11.76           C  
ATOM    301  CG2 ILE A  61      34.337 -20.069  -8.235  1.00 13.49           C  
ATOM    302  CD1 ILE A  61      32.359 -19.041  -6.269  1.00 13.72           C  
ATOM    303  N   MET A  62      33.721 -18.882 -11.572  1.00 21.25           N  
ATOM    304  CA  MET A  62      34.048 -19.563 -12.815  1.00 26.19           C  
ATOM    305  C   MET A  62      35.038 -18.750 -13.618  1.00 26.57           C  
ATOM    306  O   MET A  62      36.084 -19.245 -14.022  1.00 27.73           O  
ATOM    307  CB  MET A  62      32.801 -19.801 -13.656  1.00 32.22           C  
ATOM    308  CG  MET A  62      32.041 -21.059 -13.299  1.00 40.65           C  
ATOM    309  SD  MET A  62      30.444 -21.159 -14.164  1.00 48.52           S  
ATOM    310  CE  MET A  62      29.206 -21.058 -12.752  1.00 48.60           C  
ATOM    311  N   THR A  63      34.719 -17.482 -13.804  1.00 26.49           N  
ATOM    312  CA  THR A  63      35.548 -16.577 -14.580  1.00 27.67           C  
ATOM    313  C   THR A  63      37.002 -16.428 -14.136  1.00 24.94           C  
ATOM    314  O   THR A  63      37.910 -16.429 -14.972  1.00 26.27           O  
ATOM    315  CB  THR A  63      34.862 -15.234 -14.662  1.00 32.91           C  
ATOM    316  OG1 THR A  63      33.709 -15.368 -15.505  1.00 36.10           O  
ATOM    317  CG2 THR A  63      35.789 -14.178 -15.199  1.00 36.18           C  
ATOM    318  N   LYS A  64      37.218 -16.302 -12.829  1.00 20.57           N  
ATOM    319  CA  LYS A  64      38.555 -16.168 -12.265  1.00 16.27           C  
ATOM    320  C   LYS A  64      39.105 -17.538 -11.869  1.00 17.22           C  
ATOM    321  O   LYS A  64      39.920 -17.652 -10.968  1.00 20.18           O  
ATOM    322  CB  LYS A  64      38.495 -15.265 -11.039  1.00 16.24           C  
ATOM    323  CG  LYS A  64      38.056 -13.868 -11.370  1.00 19.29           C  
ATOM    324  CD  LYS A  64      38.081 -12.988 -10.149  1.00 22.86           C  
ATOM    325  CE  LYS A  64      39.487 -12.826  -9.614  1.00 26.64           C  
ATOM    326  NZ  LYS A  64      39.479 -11.939  -8.409  1.00 29.52           N  
ATOM    327  N   ARG A  65      38.631 -18.575 -12.545  1.00 17.49           N  
ATOM    328  CA  ARG A  65      38.997 -19.964 -12.295  1.00 14.49           C  
ATOM    329  C   ARG A  65      39.410 -20.287 -10.874  1.00 14.03           C  
ATOM    330  O   ARG A  65      40.351 -21.044 -10.665  1.00 17.94           O  
ATOM    331  CB  ARG A  65      40.070 -20.431 -13.261  1.00 14.75           C  
ATOM    332  CG  ARG A  65      39.838 -20.028 -14.703  1.00 17.60           C  
ATOM    333  CD  ARG A  65      40.309 -21.126 -15.650  1.00 22.61           C  
ATOM    334  NE  ARG A  65      41.729 -21.456 -15.510  1.00 28.14           N  
ATOM    335  CZ  ARG A  65      42.190 -22.697 -15.392  1.00 34.90           C  
ATOM    336  NH1 ARG A  65      41.349 -23.723 -15.395  1.00 38.13           N  
ATOM    337  NH2 ARG A  65      43.493 -22.920 -15.289  1.00 38.07           N  
ATOM    338  N   LEU A  66      38.652 -19.796  -9.898  1.00 10.97           N  
ATOM    339  CA  LEU A  66      38.983 -20.045  -8.498  1.00  9.06           C  
ATOM    340  C   LEU A  66      38.692 -21.446  -7.968  1.00  9.71           C  
ATOM    341  O   LEU A  66      38.925 -21.724  -6.780  1.00 13.68           O  
ATOM    342  CB  LEU A  66      38.297 -19.034  -7.589  1.00  7.18           C  
ATOM    343  CG  LEU A  66      38.524 -17.554  -7.819  1.00  6.89           C  
ATOM    344  CD1 LEU A  66      37.780 -16.767  -6.748  1.00  5.18           C  
ATOM    345  CD2 LEU A  66      40.002 -17.272  -7.753  1.00  8.57           C  
ATOM    346  N   TYR A  67      38.179 -22.331  -8.806  1.00  8.57           N  
ATOM    347  CA  TYR A  67      37.861 -23.681  -8.338  1.00 12.51           C  
ATOM    348  C   TYR A  67      39.016 -24.624  -8.635  1.00 16.43           C  
ATOM    349  O   TYR A  67      39.865 -24.328  -9.477  1.00 19.05           O  
ATOM    350  CB  TYR A  67      36.567 -24.197  -8.995  1.00 11.42           C  
ATOM    351  CG  TYR A  67      36.529 -23.987 -10.501  1.00 15.91           C  
ATOM    352  CD1 TYR A  67      37.095 -24.922 -11.377  1.00 18.44           C  
ATOM    353  CD2 TYR A  67      35.967 -22.837 -11.052  1.00 17.76           C  
ATOM    354  CE1 TYR A  67      37.102 -24.717 -12.744  1.00 17.83           C  
ATOM    355  CE2 TYR A  67      35.975 -22.623 -12.422  1.00 19.57           C  
ATOM    356  CZ  TYR A  67      36.544 -23.567 -13.259  1.00 20.81           C  
ATOM    357  OH  TYR A  67      36.557 -23.357 -14.625  1.00 26.44           O  
ATOM    358  N   ASP A  68      39.070 -25.746  -7.926  1.00 16.73           N  
ATOM    359  CA  ASP A  68      40.119 -26.715  -8.163  1.00 19.56           C  
ATOM    360  C   ASP A  68      39.798 -27.404  -9.468  1.00 24.40           C  
ATOM    361  O   ASP A  68      38.656 -27.763  -9.727  1.00 28.26           O  
ATOM    362  CB  ASP A  68      40.187 -27.747  -7.054  1.00 19.74           C  
ATOM    363  CG  ASP A  68      41.407 -28.627  -7.173  1.00 19.84           C  
ATOM    364  OD1 ASP A  68      42.476 -28.209  -6.693  1.00 22.77           O  
ATOM    365  OD2 ASP A  68      41.311 -29.715  -7.763  1.00 19.07           O  
ATOM    366  N   GLU A  69      40.820 -27.662 -10.259  1.00 26.47           N  
ATOM    367  CA  GLU A  69      40.616 -28.281 -11.551  1.00 26.24           C  
ATOM    368  C   GLU A  69      40.161 -29.725 -11.510  1.00 27.23           C  
ATOM    369  O   GLU A  69      39.405 -30.182 -12.373  1.00 26.72           O  
ATOM    370  CB  GLU A  69      41.887 -28.180 -12.355  1.00 27.13           C  
ATOM    371  CG  GLU A  69      41.645 -28.315 -13.830  1.00 29.09           C  
ATOM    372  CD  GLU A  69      41.219 -27.024 -14.466  1.00 30.90           C  
ATOM    373  OE1 GLU A  69      41.178 -25.975 -13.769  1.00 30.71           O  
ATOM    374  OE2 GLU A  69      40.941 -27.076 -15.679  1.00 33.03           O  
ATOM    375  N   LYS A  70      40.657 -30.461 -10.529  1.00 30.01           N  
ATOM    376  CA  LYS A  70      40.297 -31.861 -10.402  1.00 34.47           C  
ATOM    377  C   LYS A  70      39.040 -32.074  -9.557  1.00 34.65           C  
ATOM    378  O   LYS A  70      38.120 -32.769  -9.971  1.00 38.08           O  
ATOM    379  CB  LYS A  70      41.490 -32.678  -9.875  1.00 40.08           C  
ATOM    380  CG  LYS A  70      42.609 -32.959 -10.934  1.00 45.97           C  
ATOM    381  CD  LYS A  70      43.362 -31.687 -11.431  1.00 50.20           C  
ATOM    382  CE  LYS A  70      44.171 -30.995 -10.309  1.00 53.12           C  
ATOM    383  NZ  LYS A  70      44.770 -29.677 -10.699  1.00 54.32           N  
ATOM    384  N   GLN A  71      39.005 -31.485  -8.371  1.00 30.70           N  
ATOM    385  CA  GLN A  71      37.857 -31.604  -7.484  1.00 27.30           C  
ATOM    386  C   GLN A  71      37.161 -30.259  -7.563  1.00 21.02           C  
ATOM    387  O   GLN A  71      37.289 -29.436  -6.666  1.00 18.60           O  
ATOM    388  CB  GLN A  71      38.352 -31.883  -6.068  1.00 32.17           C  
ATOM    389  CG  GLN A  71      38.983 -33.246  -5.920  1.00 37.01           C  
ATOM    390  CD  GLN A  71      37.950 -34.357  -5.964  1.00 43.82           C  
ATOM    391  OE1 GLN A  71      37.068 -34.430  -5.110  1.00 47.96           O  
ATOM    392  NE2 GLN A  71      38.040 -35.215  -6.968  1.00 44.79           N  
ATOM    393  N   GLN A  72      36.399 -30.056  -8.633  1.00 20.30           N  
ATOM    394  CA  GLN A  72      35.719 -28.784  -8.896  1.00 16.73           C  
ATOM    395  C   GLN A  72      34.859 -28.192  -7.812  1.00 15.22           C  
ATOM    396  O   GLN A  72      34.446 -27.045  -7.919  1.00 17.63           O  
ATOM    397  CB  GLN A  72      34.930 -28.851 -10.190  1.00 16.67           C  
ATOM    398  CG  GLN A  72      35.723 -29.443 -11.328  1.00 19.27           C  
ATOM    399  CD  GLN A  72      35.798 -28.544 -12.540  1.00 22.33           C  
ATOM    400  OE1 GLN A  72      34.920 -27.711 -12.792  1.00 22.68           O  
ATOM    401  NE2 GLN A  72      36.844 -28.726 -13.323  1.00 25.00           N  
ATOM    402  N   HIS A  73      34.592 -28.958  -6.763  1.00 14.02           N  
ATOM    403  CA  HIS A  73      33.791 -28.456  -5.652  1.00 13.96           C  
ATOM    404  C   HIS A  73      34.620 -27.629  -4.661  1.00 15.64           C  
ATOM    405  O   HIS A  73      34.056 -27.010  -3.742  1.00 17.41           O  
ATOM    406  CB  HIS A  73      33.074 -29.604  -4.931  1.00 14.80           C  
ATOM    407  CG  HIS A  73      33.991 -30.572  -4.261  1.00 16.64           C  
ATOM    408  ND1 HIS A  73      34.318 -30.488  -2.924  1.00 18.85           N  
ATOM    409  CD2 HIS A  73      34.639 -31.664  -4.736  1.00 15.41           C  
ATOM    410  CE1 HIS A  73      35.124 -31.485  -2.604  1.00 16.68           C  
ATOM    411  NE2 HIS A  73      35.335 -32.212  -3.686  1.00 15.48           N  
ATOM    412  N   ILE A  74      35.950 -27.646  -4.831  1.00 12.87           N  
ATOM    413  CA  ILE A  74      36.865 -26.894  -3.970  1.00  9.08           C  
ATOM    414  C   ILE A  74      37.119 -25.524  -4.577  1.00  5.48           C  
ATOM    415  O   ILE A  74      37.492 -25.418  -5.727  1.00  8.13           O  
ATOM    416  CB  ILE A  74      38.243 -27.630  -3.779  1.00 12.08           C  
ATOM    417  CG1 ILE A  74      38.039 -29.024  -3.161  1.00 11.72           C  
ATOM    418  CG2 ILE A  74      39.174 -26.832  -2.801  1.00 10.17           C  
ATOM    419  CD1 ILE A  74      37.636 -28.963  -1.719  1.00 10.84           C  
ATOM    420  N   VAL A  75      36.889 -24.475  -3.812  1.00  2.49           N  
ATOM    421  CA  VAL A  75      37.130 -23.129  -4.293  1.00  4.31           C  
ATOM    422  C   VAL A  75      38.140 -22.513  -3.333  1.00  9.01           C  
ATOM    423  O   VAL A  75      38.054 -22.707  -2.115  1.00 10.49           O  
ATOM    424  CB  VAL A  75      35.840 -22.317  -4.362  1.00  3.43           C  
ATOM    425  CG1 VAL A  75      36.107 -20.894  -4.774  1.00  1.00           C  
ATOM    426  CG2 VAL A  75      34.913 -22.964  -5.355  1.00  4.30           C  
ATOM    427  N   TYR A  76      39.133 -21.847  -3.916  1.00 10.57           N  
ATOM    428  CA  TYR A  76      40.251 -21.237  -3.201  1.00  9.82           C  
ATOM    429  C   TYR A  76      40.091 -19.749  -3.237  1.00 13.54           C  
ATOM    430  O   TYR A  76      40.024 -19.179  -4.315  1.00 17.59           O  
ATOM    431  CB  TYR A  76      41.537 -21.582  -3.943  1.00  6.09           C  
ATOM    432  CG  TYR A  76      41.844 -23.073  -4.072  1.00  2.63           C  
ATOM    433  CD1 TYR A  76      42.331 -23.796  -2.995  1.00  1.00           C  
ATOM    434  CD2 TYR A  76      41.733 -23.719  -5.294  1.00  1.49           C  
ATOM    435  CE1 TYR A  76      42.714 -25.101  -3.136  1.00  1.00           C  
ATOM    436  CE2 TYR A  76      42.105 -25.032  -5.437  1.00  1.00           C  
ATOM    437  CZ  TYR A  76      42.600 -25.715  -4.354  1.00  1.93           C  
ATOM    438  OH  TYR A  76      43.024 -27.023  -4.493  1.00  5.30           O  
ATOM    439  N   CYS A  77      40.135 -19.090  -2.089  1.00 15.15           N  
ATOM    440  CA  CYS A  77      39.927 -17.653  -2.094  1.00 17.24           C  
ATOM    441  C   CYS A  77      40.798 -16.855  -1.166  1.00 19.10           C  
ATOM    442  O   CYS A  77      40.527 -15.676  -0.938  1.00 15.49           O  
ATOM    443  CB  CYS A  77      38.449 -17.351  -1.822  1.00 18.53           C  
ATOM    444  SG  CYS A  77      37.662 -18.378  -0.536  1.00 18.57           S  
ATOM    445  N   SER A  78      41.895 -17.469  -0.730  1.00 26.56           N  
ATOM    446  CA  SER A  78      42.874 -16.862   0.185  1.00 33.89           C  
ATOM    447  C   SER A  78      43.454 -15.587  -0.381  1.00 38.51           C  
ATOM    448  O   SER A  78      43.584 -14.563   0.294  1.00 42.17           O  
ATOM    449  CB  SER A  78      44.009 -17.847   0.444  1.00 34.90           C  
ATOM    450  OG  SER A  78      44.316 -18.547  -0.754  1.00 37.68           O  
ATOM    451  N   ASN A  79      43.779 -15.664  -1.653  1.00 39.64           N  
ATOM    452  CA  ASN A  79      44.366 -14.559  -2.394  1.00 43.62           C  
ATOM    453  C   ASN A  79      43.268 -13.694  -2.983  1.00 40.29           C  
ATOM    454  O   ASN A  79      43.551 -12.851  -3.836  1.00 39.90           O  
ATOM    455  CB  ASN A  79      45.102 -15.171  -3.579  1.00 49.71           C  
ATOM    456  CG  ASN A  79      44.178 -16.081  -4.420  1.00 53.95           C  
ATOM    457  OD1 ASN A  79      43.807 -17.195  -3.989  1.00 55.43           O  
ATOM    458  ND2 ASN A  79      43.749 -15.579  -5.581  1.00 54.43           N  
ATOM    459  N   ASP A  80      42.035 -13.869  -2.512  1.00 36.87           N  
ATOM    460  CA  ASP A  80      40.908 -13.190  -3.130  1.00 29.52           C  
ATOM    461  C   ASP A  80      39.959 -12.435  -2.231  1.00 27.60           C  
ATOM    462  O   ASP A  80      39.762 -12.820  -1.095  1.00 26.69           O  
ATOM    463  CB  ASP A  80      40.117 -14.258  -3.882  1.00 25.03           C  
ATOM    464  CG  ASP A  80      39.365 -13.714  -5.050  1.00 22.21           C  
ATOM    465  OD1 ASP A  80      40.031 -13.307  -6.019  1.00 22.42           O  
ATOM    466  OD2 ASP A  80      38.113 -13.724  -5.006  1.00 20.56           O  
ATOM    467  N   LEU A  81      39.310 -11.414  -2.804  1.00 28.92           N  
ATOM    468  CA  LEU A  81      38.294 -10.583  -2.133  1.00 30.59           C  
ATOM    469  C   LEU A  81      37.167 -11.478  -1.609  1.00 27.06           C  
ATOM    470  O   LEU A  81      36.514 -11.165  -0.620  1.00 27.06           O  
ATOM    471  CB  LEU A  81      37.703  -9.566  -3.128  1.00 37.34           C  
ATOM    472  CG  LEU A  81      36.292  -8.950  -2.964  1.00 42.99           C  
ATOM    473  CD1 LEU A  81      36.205  -7.965  -1.782  1.00 44.45           C  
ATOM    474  CD2 LEU A  81      35.909  -8.232  -4.262  1.00 43.58           C  
ATOM    475  N   LEU A  82      36.928 -12.576  -2.314  1.00 23.26           N  
ATOM    476  CA  LEU A  82      35.923 -13.537  -1.940  1.00 18.87           C  
ATOM    477  C   LEU A  82      36.295 -14.064  -0.568  1.00 20.31           C  
ATOM    478  O   LEU A  82      35.435 -14.279   0.266  1.00 22.63           O  
ATOM    479  CB  LEU A  82      35.927 -14.665  -2.956  1.00 18.68           C  
ATOM    480  CG  LEU A  82      34.818 -15.699  -2.920  1.00 16.28           C  
ATOM    481  CD1 LEU A  82      33.477 -14.974  -2.843  1.00 17.88           C  
ATOM    482  CD2 LEU A  82      34.923 -16.545  -4.168  1.00 11.40           C  
ATOM    483  N   GLY A  83      37.587 -14.250  -0.331  1.00 21.34           N  
ATOM    484  CA  GLY A  83      38.059 -14.747   0.954  1.00 19.72           C  
ATOM    485  C   GLY A  83      37.901 -13.695   2.031  1.00 22.61           C  
ATOM    486  O   GLY A  83      37.665 -14.045   3.183  1.00 25.72           O  
ATOM    487  N   ASP A  84      38.074 -12.416   1.681  1.00 24.08           N  
ATOM    488  CA  ASP A  84      37.917 -11.315   2.639  1.00 27.82           C  
ATOM    489  C   ASP A  84      36.469 -11.369   3.053  1.00 32.42           C  
ATOM    490  O   ASP A  84      36.142 -11.324   4.234  1.00 36.61           O  
ATOM    491  CB  ASP A  84      38.150  -9.953   1.978  1.00 31.47           C  
ATOM    492  CG  ASP A  84      39.600  -9.712   1.592  1.00 35.83           C  
ATOM    493  OD1 ASP A  84      40.356 -10.688   1.379  1.00 38.48           O  
ATOM    494  OD2 ASP A  84      39.984  -8.528   1.482  1.00 36.32           O  
ATOM    495  N   LEU A  85      35.621 -11.493   2.035  1.00 33.28           N  
ATOM    496  CA  LEU A  85      34.172 -11.582   2.136  1.00 31.28           C  
ATOM    497  C   LEU A  85      33.722 -12.742   3.012  1.00 28.59           C  
ATOM    498  O   LEU A  85      33.153 -12.529   4.074  1.00 32.02           O  
ATOM    499  CB  LEU A  85      33.599 -11.783   0.736  1.00 32.23           C  
ATOM    500  CG  LEU A  85      32.135 -11.469   0.508  1.00 33.98           C  
ATOM    501  CD1 LEU A  85      32.041  -9.998   0.291  1.00 34.37           C  
ATOM    502  CD2 LEU A  85      31.618 -12.202  -0.714  1.00 34.90           C  
ATOM    503  N   PHE A  86      33.970 -13.968   2.563  1.00 24.06           N  
ATOM    504  CA  PHE A  86      33.565 -15.157   3.314  1.00 22.65           C  
ATOM    505  C   PHE A  86      34.372 -15.379   4.573  1.00 20.98           C  
ATOM    506  O   PHE A  86      33.963 -16.148   5.432  1.00 21.83           O  
ATOM    507  CB  PHE A  86      33.643 -16.418   2.440  1.00 23.60           C  
ATOM    508  CG  PHE A  86      32.600 -16.475   1.346  1.00 25.25           C  
ATOM    509  CD1 PHE A  86      31.609 -15.496   1.245  1.00 26.07           C  
ATOM    510  CD2 PHE A  86      32.587 -17.519   0.439  1.00 26.99           C  
ATOM    511  CE1 PHE A  86      30.628 -15.561   0.261  1.00 26.66           C  
ATOM    512  CE2 PHE A  86      31.605 -17.596  -0.552  1.00 27.82           C  
ATOM    513  CZ  PHE A  86      30.627 -16.614  -0.638  1.00 28.09           C  
ATOM    514  N   GLY A  87      35.528 -14.727   4.660  1.00 20.07           N  
ATOM    515  CA  GLY A  87      36.406 -14.864   5.809  1.00 19.32           C  
ATOM    516  C   GLY A  87      37.016 -16.250   5.963  1.00 20.03           C  
ATOM    517  O   GLY A  87      37.169 -16.739   7.091  1.00 20.31           O  
ATOM    518  N   VAL A  88      37.337 -16.909   4.848  1.00 19.28           N  
ATOM    519  CA  VAL A  88      37.939 -18.256   4.889  1.00 18.20           C  
ATOM    520  C   VAL A  88      38.922 -18.424   3.734  1.00 19.23           C  
ATOM    521  O   VAL A  88      38.795 -17.764   2.700  1.00 22.03           O  
ATOM    522  CB  VAL A  88      36.879 -19.420   4.797  1.00 16.54           C  
ATOM    523  CG1 VAL A  88      36.041 -19.490   6.046  1.00 13.24           C  
ATOM    524  CG2 VAL A  88      36.008 -19.285   3.534  1.00 16.74           C  
ATOM    525  N   PRO A  89      39.936 -19.292   3.910  1.00 17.26           N  
ATOM    526  CA  PRO A  89      40.940 -19.547   2.873  1.00 15.01           C  
ATOM    527  C   PRO A  89      40.341 -20.305   1.690  1.00 16.26           C  
ATOM    528  O   PRO A  89      40.595 -19.957   0.536  1.00 20.02           O  
ATOM    529  CB  PRO A  89      41.987 -20.374   3.615  1.00 14.47           C  
ATOM    530  CG  PRO A  89      41.193 -21.108   4.634  1.00 14.57           C  
ATOM    531  CD  PRO A  89      40.272 -20.026   5.142  1.00 16.66           C  
ATOM    532  N   SER A  90      39.537 -21.327   1.968  1.00 14.88           N  
ATOM    533  CA  SER A  90      38.893 -22.107   0.918  1.00 15.30           C  
ATOM    534  C   SER A  90      37.525 -22.660   1.386  1.00 17.77           C  
ATOM    535  O   SER A  90      37.172 -22.532   2.562  1.00 17.21           O  
ATOM    536  CB  SER A  90      39.825 -23.238   0.476  1.00 13.07           C  
ATOM    537  OG  SER A  90      40.003 -24.201   1.499  1.00 10.50           O  
ATOM    538  N   PHE A  91      36.747 -23.242   0.471  1.00 17.71           N  
ATOM    539  CA  PHE A  91      35.443 -23.810   0.832  1.00 13.01           C  
ATOM    540  C   PHE A  91      34.952 -24.861  -0.174  1.00 13.62           C  
ATOM    541  O   PHE A  91      35.483 -24.952  -1.278  1.00 14.61           O  
ATOM    542  CB  PHE A  91      34.398 -22.696   1.069  1.00  9.68           C  
ATOM    543  CG  PHE A  91      34.044 -21.860  -0.158  1.00  5.81           C  
ATOM    544  CD1 PHE A  91      33.094 -22.293  -1.067  1.00  6.55           C  
ATOM    545  CD2 PHE A  91      34.579 -20.596  -0.338  1.00  5.77           C  
ATOM    546  CE1 PHE A  91      32.671 -21.477  -2.129  1.00  5.62           C  
ATOM    547  CE2 PHE A  91      34.164 -19.782  -1.392  1.00  5.00           C  
ATOM    548  CZ  PHE A  91      33.209 -20.230  -2.280  1.00  6.43           C  
ATOM    549  N   SER A  92      34.014 -25.714   0.241  1.00 13.98           N  
ATOM    550  CA  SER A  92      33.450 -26.746  -0.627  1.00 12.99           C  
ATOM    551  C   SER A  92      32.038 -26.359  -0.998  1.00 14.71           C  
ATOM    552  O   SER A  92      31.181 -26.253  -0.122  1.00 15.05           O  
ATOM    553  CB  SER A  92      33.399 -28.074   0.099  1.00 15.88           C  
ATOM    554  OG  SER A  92      32.677 -29.051  -0.652  1.00 19.78           O  
ATOM    555  N   VAL A  93      31.773 -26.173  -2.288  1.00 17.37           N  
ATOM    556  CA  VAL A  93      30.426 -25.784  -2.700  1.00 18.30           C  
ATOM    557  C   VAL A  93      29.429 -26.853  -2.295  1.00 20.89           C  
ATOM    558  O   VAL A  93      28.225 -26.606  -2.273  1.00 25.39           O  
ATOM    559  CB  VAL A  93      30.275 -25.513  -4.223  1.00 15.03           C  
ATOM    560  CG1 VAL A  93      31.333 -24.544  -4.718  1.00 16.89           C  
ATOM    561  CG2 VAL A  93      30.274 -26.792  -4.990  1.00 12.62           C  
ATOM    562  N   LYS A  94      29.934 -28.030  -1.951  1.00 16.49           N  
ATOM    563  CA  LYS A  94      29.078 -29.107  -1.549  1.00 15.72           C  
ATOM    564  C   LYS A  94      28.532 -28.949  -0.138  1.00 19.47           C  
ATOM    565  O   LYS A  94      27.718 -29.751   0.296  1.00 21.41           O  
ATOM    566  CB  LYS A  94      29.817 -30.419  -1.646  1.00 17.53           C  
ATOM    567  CG  LYS A  94      30.080 -30.892  -3.033  1.00 19.31           C  
ATOM    568  CD  LYS A  94      30.722 -32.253  -2.946  1.00 23.17           C  
ATOM    569  CE  LYS A  94      30.952 -32.844  -4.316  1.00 28.08           C  
ATOM    570  NZ  LYS A  94      31.566 -34.209  -4.222  1.00 30.82           N  
ATOM    571  N   GLU A  95      28.985 -27.954   0.609  1.00 22.29           N  
ATOM    572  CA  GLU A  95      28.477 -27.767   1.956  1.00 23.82           C  
ATOM    573  C   GLU A  95      27.485 -26.624   1.876  1.00 22.81           C  
ATOM    574  O   GLU A  95      27.821 -25.478   2.202  1.00 22.39           O  
ATOM    575  CB  GLU A  95      29.601 -27.367   2.890  1.00 31.07           C  
ATOM    576  CG  GLU A  95      30.820 -28.239   2.853  1.00 37.59           C  
ATOM    577  CD  GLU A  95      31.989 -27.593   3.595  1.00 43.46           C  
ATOM    578  OE1 GLU A  95      32.296 -26.392   3.349  1.00 43.63           O  
ATOM    579  OE2 GLU A  95      32.599 -28.297   4.429  1.00 46.58           O  
ATOM    580  N   HIS A  96      26.252 -26.947   1.488  1.00 21.86           N  
ATOM    581  CA  HIS A  96      25.184 -25.952   1.309  1.00 18.69           C  
ATOM    582  C   HIS A  96      24.900 -25.057   2.510  1.00 18.22           C  
ATOM    583  O   HIS A  96      24.782 -23.842   2.369  1.00 18.94           O  
ATOM    584  CB  HIS A  96      23.890 -26.617   0.839  1.00 17.58           C  
ATOM    585  CG  HIS A  96      24.017 -27.365  -0.457  1.00 16.90           C  
ATOM    586  ND1 HIS A  96      24.923 -27.019  -1.434  1.00 15.62           N  
ATOM    587  CD2 HIS A  96      23.371 -28.460  -0.919  1.00 18.23           C  
ATOM    588  CE1 HIS A  96      24.839 -27.875  -2.440  1.00 16.07           C  
ATOM    589  NE2 HIS A  96      23.904 -28.759  -2.151  1.00 17.70           N  
ATOM    590  N   ARG A  97      24.798 -25.638   3.694  1.00 17.37           N  
ATOM    591  CA  ARG A  97      24.525 -24.832   4.873  1.00 15.09           C  
ATOM    592  C   ARG A  97      25.645 -23.820   5.118  1.00 16.62           C  
ATOM    593  O   ARG A  97      25.395 -22.678   5.494  1.00 16.57           O  
ATOM    594  CB  ARG A  97      24.314 -25.730   6.093  1.00 12.02           C  
ATOM    595  CG  ARG A  97      23.811 -24.994   7.323  1.00 11.33           C  
ATOM    596  CD  ARG A  97      23.642 -25.948   8.472  1.00 12.38           C  
ATOM    597  NE  ARG A  97      22.697 -27.032   8.216  1.00 15.31           N  
ATOM    598  CZ  ARG A  97      21.422 -27.040   8.630  1.00 17.96           C  
ATOM    599  NH1 ARG A  97      20.921 -26.012   9.312  1.00 15.89           N  
ATOM    600  NH2 ARG A  97      20.666 -28.118   8.441  1.00 18.19           N  
ATOM    601  N   LYS A  98      26.883 -24.219   4.861  1.00 19.25           N  
ATOM    602  CA  LYS A  98      27.998 -23.312   5.072  1.00 20.50           C  
ATOM    603  C   LYS A  98      27.983 -22.184   4.079  1.00 18.09           C  
ATOM    604  O   LYS A  98      28.003 -21.023   4.478  1.00 20.14           O  
ATOM    605  CB  LYS A  98      29.324 -24.039   4.974  1.00 27.06           C  
ATOM    606  CG  LYS A  98      29.808 -24.623   6.264  1.00 33.25           C  
ATOM    607  CD  LYS A  98      31.240 -25.071   6.082  1.00 39.81           C  
ATOM    608  CE  LYS A  98      32.090 -23.947   5.472  1.00 44.47           C  
ATOM    609  NZ  LYS A  98      32.296 -22.815   6.427  1.00 47.34           N  
ATOM    610  N   ILE A  99      27.949 -22.524   2.788  1.00 15.40           N  
ATOM    611  CA  ILE A  99      27.938 -21.523   1.717  1.00 15.01           C  
ATOM    612  C   ILE A  99      26.802 -20.529   1.923  1.00 17.44           C  
ATOM    613  O   ILE A  99      27.014 -19.307   1.926  1.00 18.10           O  
ATOM    614  CB  ILE A  99      27.728 -22.153   0.328  1.00 15.52           C  
ATOM    615  CG1 ILE A  99      28.645 -23.348   0.118  1.00 16.84           C  
ATOM    616  CG2 ILE A  99      28.032 -21.138  -0.728  1.00 16.45           C  
ATOM    617  CD1 ILE A  99      30.108 -22.999   0.168  1.00 18.50           C  
ATOM    618  N   TYR A 100      25.598 -21.070   2.126  1.00 19.20           N  
ATOM    619  CA  TYR A 100      24.403 -20.267   2.326  1.00 18.81           C  
ATOM    620  C   TYR A 100      24.547 -19.345   3.510  1.00 16.22           C  
ATOM    621  O   TYR A 100      24.117 -18.185   3.454  1.00 19.82           O  
ATOM    622  CB  TYR A 100      23.190 -21.153   2.496  1.00 24.97           C  
ATOM    623  CG  TYR A 100      21.898 -20.417   2.284  1.00 30.83           C  
ATOM    624  CD1 TYR A 100      21.482 -20.044   1.004  1.00 31.83           C  
ATOM    625  CD2 TYR A 100      21.095 -20.079   3.366  1.00 35.02           C  
ATOM    626  CE1 TYR A 100      20.306 -19.348   0.814  1.00 34.30           C  
ATOM    627  CE2 TYR A 100      19.917 -19.384   3.192  1.00 37.14           C  
ATOM    628  CZ  TYR A 100      19.526 -19.019   1.921  1.00 38.42           C  
ATOM    629  OH  TYR A 100      18.367 -18.281   1.803  1.00 42.34           O  
ATOM    630  N   THR A 101      25.186 -19.830   4.566  1.00 11.06           N  
ATOM    631  CA  THR A 101      25.388 -18.998   5.738  1.00 13.10           C  
ATOM    632  C   THR A 101      26.369 -17.858   5.493  1.00 11.97           C  
ATOM    633  O   THR A 101      26.133 -16.706   5.874  1.00 11.20           O  
ATOM    634  CB  THR A 101      25.880 -19.837   6.946  1.00 19.30           C  
ATOM    635  OG1 THR A 101      24.880 -20.809   7.296  1.00 23.86           O  
ATOM    636  CG2 THR A 101      26.154 -18.938   8.162  1.00 18.32           C  
ATOM    637  N   MET A 102      27.494 -18.180   4.873  1.00 13.67           N  
ATOM    638  CA  MET A 102      28.506 -17.169   4.622  1.00 14.03           C  
ATOM    639  C   MET A 102      27.932 -16.089   3.752  1.00 13.43           C  
ATOM    640  O   MET A 102      28.082 -14.910   4.059  1.00 13.36           O  
ATOM    641  CB  MET A 102      29.748 -17.787   3.990  1.00 15.52           C  
ATOM    642  CG  MET A 102      30.469 -18.757   4.907  1.00 16.74           C  
ATOM    643  SD  MET A 102      32.005 -19.317   4.198  1.00 19.43           S  
ATOM    644  CE  MET A 102      31.533 -20.724   3.326  1.00 17.64           C  
ATOM    645  N   ILE A 103      27.232 -16.506   2.697  1.00 15.07           N  
ATOM    646  CA  ILE A 103      26.593 -15.577   1.774  1.00 17.58           C  
ATOM    647  C   ILE A 103      25.575 -14.690   2.508  1.00 20.37           C  
ATOM    648  O   ILE A 103      25.527 -13.486   2.277  1.00 18.95           O  
ATOM    649  CB  ILE A 103      25.883 -16.321   0.634  1.00 17.88           C  
ATOM    650  CG1 ILE A 103      26.896 -17.111  -0.193  1.00 17.49           C  
ATOM    651  CG2 ILE A 103      25.152 -15.335  -0.267  1.00 17.47           C  
ATOM    652  CD1 ILE A 103      26.266 -17.985  -1.286  1.00 15.46           C  
ATOM    653  N   TYR A 104      24.791 -15.269   3.417  1.00 26.57           N  
ATOM    654  CA  TYR A 104      23.812 -14.487   4.168  1.00 30.90           C  
ATOM    655  C   TYR A 104      24.384 -13.445   5.095  1.00 31.73           C  
ATOM    656  O   TYR A 104      23.786 -12.399   5.278  1.00 31.18           O  
ATOM    657  CB  TYR A 104      22.841 -15.375   4.922  1.00 36.22           C  
ATOM    658  CG  TYR A 104      21.502 -15.350   4.254  1.00 43.21           C  
ATOM    659  CD1 TYR A 104      21.219 -16.222   3.208  1.00 46.10           C  
ATOM    660  CD2 TYR A 104      20.546 -14.381   4.591  1.00 47.20           C  
ATOM    661  CE1 TYR A 104      20.031 -16.134   2.505  1.00 49.00           C  
ATOM    662  CE2 TYR A 104      19.338 -14.286   3.885  1.00 50.18           C  
ATOM    663  CZ  TYR A 104      19.091 -15.175   2.840  1.00 51.57           C  
ATOM    664  OH  TYR A 104      17.904 -15.133   2.138  1.00 53.78           O  
ATOM    665  N   ARG A 105      25.558 -13.712   5.652  1.00 35.53           N  
ATOM    666  CA  ARG A 105      26.227 -12.764   6.549  1.00 34.37           C  
ATOM    667  C   ARG A 105      26.539 -11.468   5.814  1.00 30.61           C  
ATOM    668  O   ARG A 105      26.762 -10.429   6.439  1.00 32.92           O  
ATOM    669  CB  ARG A 105      27.538 -13.365   7.096  1.00 38.15           C  
ATOM    670  CG  ARG A 105      27.350 -14.668   7.876  1.00 43.81           C  
ATOM    671  CD  ARG A 105      28.516 -14.975   8.808  1.00 49.18           C  
ATOM    672  NE  ARG A 105      29.578 -15.760   8.174  1.00 54.51           N  
ATOM    673  CZ  ARG A 105      30.684 -15.253   7.623  1.00 57.35           C  
ATOM    674  NH1 ARG A 105      30.904 -13.938   7.603  1.00 58.70           N  
ATOM    675  NH2 ARG A 105      31.589 -16.072   7.106  1.00 57.31           N  
ATOM    676  N   ASN A 106      26.546 -11.532   4.487  1.00 27.25           N  
ATOM    677  CA  ASN A 106      26.864 -10.377   3.657  1.00 27.34           C  
ATOM    678  C   ASN A 106      25.679  -9.699   3.022  1.00 27.34           C  
ATOM    679  O   ASN A 106      25.842  -8.848   2.149  1.00 25.54           O  
ATOM    680  CB  ASN A 106      27.853 -10.770   2.563  1.00 30.39           C  
ATOM    681  CG  ASN A 106      29.258 -10.946   3.091  1.00 30.78           C  
ATOM    682  OD1 ASN A 106      29.664 -12.058   3.420  1.00 31.77           O  
ATOM    683  ND2 ASN A 106      30.002  -9.839   3.207  1.00 28.54           N  
ATOM    684  N   LEU A 107      24.487 -10.065   3.465  1.00 29.67           N  
ATOM    685  CA  LEU A 107      23.276  -9.482   2.929  1.00 28.94           C  
ATOM    686  C   LEU A 107      22.467  -8.866   4.044  1.00 30.37           C  
ATOM    687  O   LEU A 107      22.684  -9.133   5.225  1.00 29.22           O  
ATOM    688  CB  LEU A 107      22.390 -10.558   2.300  1.00 25.42           C  
ATOM    689  CG  LEU A 107      22.846 -11.433   1.151  1.00 22.75           C  
ATOM    690  CD1 LEU A 107      21.897 -12.599   1.086  1.00 23.42           C  
ATOM    691  CD2 LEU A 107      22.903 -10.680  -0.153  1.00 20.95           C  
ATOM    692  N   VAL A 108      21.459  -8.118   3.631  1.00 32.60           N  
ATOM    693  CA  VAL A 108      20.518  -7.502   4.535  1.00 34.59           C  
ATOM    694  C   VAL A 108      19.180  -7.624   3.820  1.00 34.90           C  
ATOM    695  O   VAL A 108      19.119  -7.559   2.589  1.00 32.74           O  
ATOM    696  CB  VAL A 108      20.914  -6.048   4.869  1.00 36.33           C  
ATOM    697  CG1 VAL A 108      21.650  -5.412   3.709  1.00 37.75           C  
ATOM    698  CG2 VAL A 108      19.690  -5.236   5.243  1.00 36.99           C  
ATOM    699  N   VAL A 109      18.143  -7.864   4.619  1.00 38.64           N  
ATOM    700  CA  VAL A 109      16.736  -8.058   4.213  1.00 40.78           C  
ATOM    701  C   VAL A 109      16.520  -9.550   3.941  1.00 42.51           C  
ATOM    702  O   VAL A 109      17.481 -10.242   3.539  1.00 42.68           O  
ATOM    703  CB  VAL A 109      16.277  -7.204   2.975  1.00 41.86           C  
ATOM    704  CG1 VAL A 109      14.762  -7.298   2.791  1.00 41.92           C  
ATOM    705  CG2 VAL A 109      16.649  -5.745   3.141  1.00 43.61           C  
TER     706      VAL A 109                                                      
ATOM    707  N   GLU B  17      32.075 -34.286 -11.853  1.00 52.56           N  
ATOM    708  CA  GLU B  17      31.206 -33.198 -11.326  1.00 50.89           C  
ATOM    709  C   GLU B  17      31.909 -31.865 -11.563  1.00 44.52           C  
ATOM    710  O   GLU B  17      32.999 -31.632 -11.049  1.00 46.37           O  
ATOM    711  CB  GLU B  17      30.912 -33.429  -9.824  1.00 55.17           C  
ATOM    712  CG  GLU B  17      32.078 -34.013  -8.980  1.00 58.48           C  
ATOM    713  CD  GLU B  17      32.839 -32.976  -8.139  1.00 60.32           C  
ATOM    714  OE1 GLU B  17      32.302 -32.543  -7.098  1.00 60.54           O  
ATOM    715  OE2 GLU B  17      33.988 -32.618  -8.498  1.00 60.63           O  
ATOM    716  N   THR B  18      31.324 -31.015 -12.390  1.00 36.89           N  
ATOM    717  CA  THR B  18      31.948 -29.737 -12.658  1.00 32.17           C  
ATOM    718  C   THR B  18      31.419 -28.661 -11.707  1.00 26.92           C  
ATOM    719  O   THR B  18      30.376 -28.848 -11.062  1.00 25.44           O  
ATOM    720  CB  THR B  18      31.733 -29.316 -14.107  1.00 33.65           C  
ATOM    721  OG1 THR B  18      30.372 -28.917 -14.291  1.00 33.86           O  
ATOM    722  CG2 THR B  18      32.040 -30.485 -15.032  1.00 35.00           C  
ATOM    723  N   PHE B  19      32.143 -27.542 -11.625  1.00 22.45           N  
ATOM    724  CA  PHE B  19      31.769 -26.453 -10.742  1.00 17.73           C  
ATOM    725  C   PHE B  19      30.344 -26.019 -10.974  1.00 21.74           C  
ATOM    726  O   PHE B  19      29.549 -25.992 -10.043  1.00 22.84           O  
ATOM    727  CB  PHE B  19      32.674 -25.245 -10.916  1.00 12.04           C  
ATOM    728  CG  PHE B  19      32.265 -24.076 -10.062  1.00 12.01           C  
ATOM    729  CD1 PHE B  19      32.438 -24.116  -8.685  1.00 12.17           C  
ATOM    730  CD2 PHE B  19      31.667 -22.955 -10.628  1.00 11.53           C  
ATOM    731  CE1 PHE B  19      32.021 -23.055  -7.889  1.00 13.45           C  
ATOM    732  CE2 PHE B  19      31.246 -21.888  -9.839  1.00 10.96           C  
ATOM    733  CZ  PHE B  19      31.421 -21.935  -8.468  1.00 11.75           C  
ATOM    734  N   SER B  20      30.030 -25.693 -12.225  1.00 24.64           N  
ATOM    735  CA  SER B  20      28.703 -25.237 -12.606  1.00 26.30           C  
ATOM    736  C   SER B  20      27.575 -26.173 -12.205  1.00 27.49           C  
ATOM    737  O   SER B  20      26.509 -25.712 -11.822  1.00 28.40           O  
ATOM    738  CB  SER B  20      28.652 -24.947 -14.093  1.00 27.55           C  
ATOM    739  OG  SER B  20      28.286 -23.593 -14.268  1.00 30.01           O  
ATOM    740  N   ASP B  21      27.809 -27.478 -12.284  1.00 27.30           N  
ATOM    741  CA  ASP B  21      26.814 -28.452 -11.882  1.00 28.79           C  
ATOM    742  C   ASP B  21      26.639 -28.352 -10.378  1.00 29.54           C  
ATOM    743  O   ASP B  21      25.519 -28.300  -9.873  1.00 33.11           O  
ATOM    744  CB  ASP B  21      27.281 -29.875 -12.205  1.00 33.00           C  
ATOM    745  CG  ASP B  21      27.479 -30.112 -13.692  1.00 38.52           C  
ATOM    746  OD1 ASP B  21      26.877 -29.358 -14.503  1.00 39.73           O  
ATOM    747  OD2 ASP B  21      28.238 -31.057 -14.042  1.00 40.32           O  
ATOM    748  N   LEU B  22      27.761 -28.324  -9.665  1.00 26.27           N  
ATOM    749  CA  LEU B  22      27.753 -28.274  -8.217  1.00 21.85           C  
ATOM    750  C   LEU B  22      27.124 -27.006  -7.657  1.00 22.11           C  
ATOM    751  O   LEU B  22      26.320 -27.064  -6.716  1.00 24.72           O  
ATOM    752  CB  LEU B  22      29.180 -28.412  -7.685  1.00 18.73           C  
ATOM    753  CG  LEU B  22      29.872 -29.738  -7.354  1.00 15.80           C  
ATOM    754  CD1 LEU B  22      29.042 -30.916  -7.782  1.00 14.53           C  
ATOM    755  CD2 LEU B  22      31.236 -29.774  -8.020  1.00 14.98           C  
ATOM    756  N   TRP B  23      27.477 -25.871  -8.251  1.00 19.21           N  
ATOM    757  CA  TRP B  23      27.021 -24.553  -7.803  1.00 20.10           C  
ATOM    758  C   TRP B  23      25.532 -24.284  -8.011  1.00 25.27           C  
ATOM    759  O   TRP B  23      24.919 -23.498  -7.297  1.00 27.09           O  
ATOM    760  CB  TRP B  23      27.892 -23.448  -8.451  1.00 14.64           C  
ATOM    761  CG  TRP B  23      27.585 -22.055  -8.009  1.00 11.74           C  
ATOM    762  CD1 TRP B  23      26.730 -21.178  -8.613  1.00 10.40           C  
ATOM    763  CD2 TRP B  23      28.090 -21.382  -6.842  1.00 11.59           C  
ATOM    764  NE1 TRP B  23      26.659 -20.002  -7.892  1.00  8.62           N  
ATOM    765  CE2 TRP B  23      27.478 -20.101  -6.799  1.00 10.54           C  
ATOM    766  CE3 TRP B  23      28.990 -21.731  -5.830  1.00 11.67           C  
ATOM    767  CZ2 TRP B  23      27.738 -19.177  -5.784  1.00 11.03           C  
ATOM    768  CZ3 TRP B  23      29.251 -20.809  -4.825  1.00 10.73           C  
ATOM    769  CH2 TRP B  23      28.623 -19.548  -4.812  1.00 10.81           C  
ATOM    770  N   LYS B  24      24.939 -24.930  -8.991  1.00 30.12           N  
ATOM    771  CA  LYS B  24      23.530 -24.716  -9.231  1.00 35.48           C  
ATOM    772  C   LYS B  24      22.646 -25.442  -8.219  1.00 35.68           C  
ATOM    773  O   LYS B  24      21.451 -25.171  -8.134  1.00 39.75           O  
ATOM    774  CB  LYS B  24      23.180 -25.083 -10.671  1.00 41.39           C  
ATOM    775  CG  LYS B  24      23.805 -24.114 -11.676  1.00 46.84           C  
ATOM    776  CD  LYS B  24      23.803 -24.649 -13.100  1.00 50.66           C  
ATOM    777  CE  LYS B  24      24.657 -23.764 -13.999  1.00 52.44           C  
ATOM    778  NZ  LYS B  24      24.754 -24.351 -15.355  1.00 53.53           N  
ATOM    779  N   LEU B  25      23.240 -26.308  -7.408  1.00 31.53           N  
ATOM    780  CA  LEU B  25      22.485 -27.035  -6.394  1.00 28.43           C  
ATOM    781  C   LEU B  25      22.403 -26.295  -5.067  1.00 27.36           C  
ATOM    782  O   LEU B  25      22.155 -26.900  -4.030  1.00 28.91           O  
ATOM    783  CB  LEU B  25      23.113 -28.401  -6.153  1.00 28.84           C  
ATOM    784  CG  LEU B  25      23.201 -29.321  -7.366  1.00 30.87           C  
ATOM    785  CD1 LEU B  25      23.891 -30.600  -6.922  1.00 30.42           C  
ATOM    786  CD2 LEU B  25      21.808 -29.605  -7.945  1.00 30.75           C  
ATOM    787  N   LEU B  26      22.657 -24.998  -5.083  1.00 28.16           N  
ATOM    788  CA  LEU B  26      22.616 -24.202  -3.862  1.00 30.58           C  
ATOM    789  C   LEU B  26      21.235 -23.583  -3.733  1.00 32.69           C  
ATOM    790  O   LEU B  26      20.630 -23.223  -4.753  1.00 33.43           O  
ATOM    791  CB  LEU B  26      23.652 -23.069  -3.918  1.00 30.27           C  
ATOM    792  CG  LEU B  26      25.131 -23.345  -4.163  1.00 29.57           C  
ATOM    793  CD1 LEU B  26      25.825 -22.010  -4.268  1.00 29.65           C  
ATOM    794  CD2 LEU B  26      25.723 -24.177  -3.038  1.00 29.90           C  
ATOM    795  N   PRO B  27      20.753 -23.387  -2.477  1.00 32.15           N  
ATOM    796  CA  PRO B  27      19.442 -22.801  -2.172  1.00 32.91           C  
ATOM    797  C   PRO B  27      19.329 -21.458  -2.877  1.00 36.74           C  
ATOM    798  O   PRO B  27      20.105 -20.538  -2.608  1.00 37.93           O  
ATOM    799  CB  PRO B  27      19.491 -22.646  -0.660  1.00 31.00           C  
ATOM    800  CG  PRO B  27      20.369 -23.774  -0.245  1.00 28.74           C  
ATOM    801  CD  PRO B  27      21.480 -23.661  -1.226  1.00 29.37           C  
ATOM    802  N   GLU B  28      18.368 -21.373  -3.794  1.00 39.68           N  
ATOM    803  CA  GLU B  28      18.139 -20.187  -4.619  1.00 41.42           C  
ATOM    804  C   GLU B  28      17.306 -19.049  -4.048  1.00 41.07           C  
ATOM    805  O   GLU B  28      17.039 -18.062  -4.733  1.00 44.94           O  
ATOM    806  CB  GLU B  28      17.603 -20.610  -5.988  1.00 43.50           C  
ATOM    807  CG  GLU B  28      16.553 -21.693  -5.923  1.00 46.41           C  
ATOM    808  CD  GLU B  28      16.395 -22.434  -7.228  1.00 50.74           C  
ATOM    809  OE1 GLU B  28      17.341 -22.426  -8.054  1.00 50.52           O  
ATOM    810  OE2 GLU B  28      15.315 -23.043  -7.418  1.00 54.75           O  
ATOM    811  N   ASN B  29      16.925 -19.172  -2.791  1.00 37.74           N  
ATOM    812  CA  ASN B  29      16.147 -18.147  -2.112  1.00 37.72           C  
ATOM    813  C   ASN B  29      16.548 -18.261  -0.669  1.00 30.80           C  
ATOM    814  O   ASN B  29      17.159 -19.284  -0.370  1.00 28.87           O  
ATOM    815  CB  ASN B  29      14.634 -18.393  -2.256  1.00 44.98           C  
ATOM    816  CG  ASN B  29      14.044 -17.784  -3.540  1.00 49.82           C  
ATOM    817  OD1 ASN B  29      14.536 -16.773  -4.071  1.00 50.99           O  
ATOM    818  ND2 ASN B  29      12.963 -18.387  -4.023  1.00 51.64           N  
ATOM    819  OXT ASN B  29      16.296 -17.351   0.139  1.00 30.36           O  
TER     820      ASN B  29                                                      
MASTER      247    0    0    5    2    0    0    6  818    2    0   11          
END                                                                             
"""


pdb1ycr : Model.Structure
pdb1ycr =
    { pdbFile = pdb1ycrString
    , chainLabels = [ "A", "B" ]
    , geometryLabels =
        [ ( "A_cartoon", False )
        , ( "A_lines", False )
        , ( "B_cartoon", False )
        , ( "B_lines", False )
        ]
    }


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
                            True
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
        pdb1ycrString
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
                            , rotamerFixActive = True
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
                            , rotamerFixActive = True
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
                            , rotamerFixActive = True
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
