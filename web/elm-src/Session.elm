module Session exposing (Msg(..), Session(..), update, view)

import Css
import Fancy
import Html
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Model
import Ports
import Tutorial
import Update
import View


type Session
    = ActiveMode Model.Model Bool
    | TutorialMode (Tutorial.Tutorial TutorialMsg)


type Msg
    = ToggleMode
    | ActiveMessage Update.Msg
    | TutorialMessage TutorialMsg


type TutorialMsg
    = PreviousSection
    | NextSection
    | CancelTutorial


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        ActiveMessage updateMsg ->
            case session of
                ActiveMode model _ ->
                    let
                        ( updatedModel, cmds ) =
                            Update.update updateMsg model
                    in
                    ( ActiveMode updatedModel True
                    , Cmd.map ActiveMessage cmds
                    )

                TutorialMode _ ->
                    ( session, Cmd.none )

        TutorialMessage CancelTutorial ->
            update ToggleMode session

        TutorialMessage tutorialMsg ->
            case session of
                TutorialMode tutorial ->
                    tutorialUpdate tutorialMsg tutorial
                        |> tutorialToSession

                ActiveMode _ _ ->
                    ( session, Cmd.none )

        ToggleMode ->
            case session of
                ActiveMode model _ ->
                    { previous = PreviousSection
                    , next = NextSection
                    , cancel = CancelTutorial
                    }
                        |> Tutorial.tutorial
                        |> tutorialToSession

                TutorialMode _ ->
                    ( ActiveMode Model.emptyModel True
                    , Ports.clearViewer ()
                    )


tutorialUpdate :
    TutorialMsg
    -> Tutorial.Tutorial TutorialMsg
    -> Tutorial.Tutorial TutorialMsg
tutorialUpdate msg tutorial =
    case msg of
        NextSection ->
            Tutorial.moveForward tutorial

        PreviousSection ->
            Tutorial.moveBackwards tutorial

        CancelTutorial ->
            tutorial


tutorialToSession :
    Tutorial.Tutorial TutorialMsg
    -> ( Session, Cmd Msg )
tutorialToSession tutorial =
    let
        (Tutorial.Tutorial _ currentSection _) =
            tutorial
    in
    ( TutorialMode tutorial, Cmd.map ActiveMessage currentSection.command )


view : Session -> Html Msg
view session =
    div []
        ([ Fancy.controlButton
            [ onClick ToggleMode
            , css
                [ Css.top (Css.calc (Css.pct 5) Css.plus (Css.px 5))
                , Css.left (Css.px 5)
                , Css.zIndex (Css.int 1001)
                , Css.position Css.absolute
                ]
            ]
            [ text "❓" ]
         ]
            ++ [ case session of
                    ActiveMode model _ ->
                        div
                            [ css
                                [ Css.fontFamilies
                                    [ "Titillium Web", "sans-serif" ]
                                ]
                            ]
                            [ Styled.map ActiveMessage <|
                                View.view model
                            ]

                    TutorialMode (Tutorial.Tutorial _ section _) ->
                        div
                            [ css
                                [ Css.fontFamilies
                                    [ "Titillium Web", "sans-serif" ]
                                ]
                            ]
                            [ Styled.map ActiveMessage <|
                                View.view section.model
                            , div
                                [ css
                                    [ Css.alignItems Css.center
                                    , Css.displayFlex
                                    , Css.height (Css.pct 100)
                                    , Css.justifyContent Css.center
                                    , Css.left Css.zero
                                    , Css.position Css.absolute
                                    , Css.top Css.zero
                                    , Css.width (Css.pct 100)
                                    , Css.zIndex (Css.int 1002)
                                    ]
                                ]
                                [ Styled.map TutorialMessage <|
                                    section.tutorialWindow
                                ]
                            ]
               ]
        )
