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
import Html.Styled.Events exposing (..)
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


tutorial : Config msg -> Tutorial msg
tutorial config =
    Tutorial [] (welcome config) <|
        List.map (\s -> s config)
            []


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


welcome : Config msg -> Section msg
welcome { previous, next, cancel } =
    { tutorialWindow =
        tutorialWindow []
            [ p []
                [ "Hello and welcome to BUDE Alanine Scan. This tutorial will "
                    ++ "guide you through the basics of the application. "
                    |> text
                ]
            , Fancy.button [ onClick cancel ] [ text "Cancel" ]
            ]
    , model = Model.emptyModel
    }


tutorialWindow : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
tutorialWindow =
    styled Styled.div
        [ Css.backgroundColor Fancy.colourPalette.c2 ]
