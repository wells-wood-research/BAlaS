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


type Tutorial msg
    = Tutorial (Sections msg) (CurrentSection msg) (Sections msg)


type alias Sections msg =
    List (Section msg)


type alias CurrentSection msg =
    Section msg


type alias Section msg =
    { tutorialWindow : Html msg
    , model : Model.Model
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
            [ modes ]


tutorialWindow : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
tutorialWindow =
    styled Styled.div
        [ Css.backgroundColor Fancy.colourPalette.c2
        , Css.boxShadow5
            Css.zero
            Css.zero
            (Css.px 10)
            Css.zero
            (Css.rgba 133 133 133 1)
        , Css.padding (Css.px 5)
        ]


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
            , Fancy.button [ disabled True ] [ text "Previous" ]
            , Fancy.button [ onClick next ] [ text "Next" ]
            , Fancy.button [ onClick cancel ] [ text "Cancel" ]
            ]
    , model = Model.emptyModel
    }


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
    }


modeExplaination : Html msg
modeExplaination =
    """BUDE Alanine Scan (BALS) has two modes of operation:

* **Scan** - performs standard computational alanine scan, where every residue
of the protein of interest (called the _ligand_ in the app) is mutated to
alanine in turn.  The interaction energy between each mutant of the _ligand_ and
another protein (called the _receptor_ in the app) is calculated using the [BUDE
all-atom force
field](http://journals.sagepub.com/doi/abs/10.1177/1094342014528252). The
difference in the energy of with wild-type sequence _ligand_/_receptor_
interaction and the mutant _ligand_/_receptor_ interaction, known as the ΔΔG, is
calculated, which quantifies the energetic contribution of the residue to
_ligand_/_receptor_ interaction.

* **Constellation** - Once a Scan job is complete you can run a Constellation
job.  In Constellation mode you can combine multiple alanine mutations and look
for clusters of _ligand_ residues that cooperatively interact with the
_receptor_ _i.e._ the sum of their individual ΔΔG values is less than the
interaction energy with the _receptor_ when all residues are mutated to alanine
simultaneously. This is useful for highlighting regions of the _ligand_ that are
key to forming the interaction with the _receptor_.
"""
        |> Markdown.toHtml []
        |> Styled.fromUnstyled
