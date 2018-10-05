module Session exposing (Msg(..), Session, SessionMode(..), update, view)

import Browser
import Browser.Navigation as Nav
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
import Url
import View


type alias Session =
    { mode : SessionMode
    , key : Nav.Key
    }


type SessionMode
    = ActiveMode Model.Model
    | TutorialMode (Tutorial.Tutorial TutorialMsg) Model.SaveState


type Msg
    = ToggleMode
    | ActiveMessage Update.Msg
    | TutorialMessage TutorialMsg
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url


type TutorialMsg
    = PreviousSection
    | NextSection
    | CancelTutorial


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        ActiveMessage updateMsg ->
            case session.mode of
                ActiveMode model ->
                    let
                        ( updatedModel, cmds ) =
                            Update.update updateMsg model
                    in
                    ( { session | mode = ActiveMode updatedModel }
                    , Cmd.map ActiveMessage cmds
                    )

                TutorialMode _ _ ->
                    ( session, Cmd.none )

        TutorialMessage CancelTutorial ->
            update ToggleMode session

        TutorialMessage tutorialMsg ->
            case session.mode of
                TutorialMode tutorial previousModel ->
                    tutorialUpdate tutorialMsg tutorial
                        |> tutorialToSession
                            (Model.saveStateToModel
                                previousModel
                            )
                            session

                ActiveMode _ ->
                    ( session, Cmd.none )

        ToggleMode ->
            case session.mode of
                ActiveMode model ->
                    { previous = PreviousSection
                    , next = NextSection
                    , cancel = CancelTutorial
                    }
                        |> Tutorial.tutorial
                        |> tutorialToSession model session

                TutorialMode _ saveState ->
                    ( { session
                        | mode =
                            ActiveMode
                                (Model.saveStateToModel
                                    saveState
                                )
                      }
                    , Ports.clearViewer ()
                    )

        LinkClicked _ ->
            ( session, Cmd.none )

        UrlChanged _ ->
            ( session, Cmd.none )


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
    Model.Model
    -> Session
    -> Tutorial.Tutorial TutorialMsg
    -> ( Session, Cmd Msg )
tutorialToSession model session tutorial =
    let
        (Tutorial.Tutorial _ currentSection _) =
            tutorial
    in
    ( { session | mode = TutorialMode tutorial (Model.modelToSaveState model) }
    , Cmd.map ActiveMessage currentSection.command
    )


view : Session -> Browser.Document Msg
view session =
    { title = "BUDE Alanine Scan"
    , body =
        body session
            |> Styled.toUnstyled
            |> List.singleton
    }


body : Session -> Html Msg
body session =
    div []
        ([ Fancy.controlButton
            [ onClick ToggleMode
            , css
                [ Css.bottom (Css.px 5)
                , Css.left (Css.px 5)
                , Css.zIndex (Css.int 900)
                , Css.position Css.absolute
                ]
            ]
            [ text "❓" ]
         ]
            ++ [ case session.mode of
                    ActiveMode model ->
                        div
                            [ css
                                [ Css.fontFamilies
                                    [ "Titillium Web", "sans-serif" ]
                                ]
                            ]
                            [ Styled.map ActiveMessage <|
                                View.view model
                            ]

                    TutorialMode (Tutorial.Tutorial _ section _) _ ->
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
