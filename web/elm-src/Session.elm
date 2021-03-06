module Session exposing (Msg(..), Session, SessionMode(..), update, view)

import Browser
import Browser.Navigation as Nav
import Css
import Dict
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
import Url.Parser as UrlP exposing ((</>), (<?>), Parser)
import Url.Parser.Query as Query
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


type Route
    = Scan String (Maybe Action)
    | Auto String (Maybe Action)
    | Residues String (Maybe Action)
    | Manual String (Maybe Action)
    | GetResults String


type Action
    = Delete
    | CopyToClipboard


routeParser : Parser (Route -> a) a
routeParser =
    UrlP.oneOf
        [ UrlP.map Scan (UrlP.s "scan" </> UrlP.string <?> actionParser)
        , UrlP.map Auto (UrlP.s "auto" </> UrlP.string <?> actionParser)
        , UrlP.map Residues (UrlP.s "residues" </> UrlP.string <?> actionParser)
        , UrlP.map Manual (UrlP.s "manual" </> UrlP.string <?> actionParser)
        , UrlP.map GetResults (UrlP.s "result-files" </> UrlP.string)
        ]


actionParser : Query.Parser (Maybe Action)
actionParser =
    Query.enum "action"
        (Dict.fromList
            [ ( "delete", Delete )
            , ( "copy-to-clipboard", CopyToClipboard )
            ]
        )


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
                    , Cmd.map ActiveMessage <|
                        Cmd.batch
                            ([ cmds
                             ]
                                ++ (case updateMsg of
                                        Update.LoadState _ ->
                                            []

                                        _ ->
                                            [ Model.saveModel updatedModel
                                                |> Ports.saveState
                                            ]
                                   )
                            )
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
                                (Just previousModel)
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
                                    (Just saveState)
                                )
                      }
                    , Ports.clearViewer ()
                    )

        LinkClicked (Browser.Internal url) ->
            resolveInternalUrl session url

        LinkClicked (Browser.External url) ->
            ( session
            , Nav.load url
            )

        UrlChanged url ->
            resolveInternalUrl session url


resolveInternalUrl : Session -> Url.Url -> ( Session, Cmd Msg )
resolveInternalUrl session url =
    case session.mode of
        ActiveMode model ->
            let
                msgToSession =
                    modelToActiveSession session model
            in
            case UrlP.parse routeParser url of
                Just (Scan jobID Nothing) ->
                    msgToSession
                        (Update.UpdateScan <|
                            Update.GetScanResults jobID
                        )

                Just (Scan jobID (Just CopyToClipboard)) ->
                    msgToSession
                        (Url.toString url
                            |> String.split "?"
                            |> List.head
                            |> Maybe.withDefault ""
                            |> Update.CopyToClipboard
                        )

                Just (Scan jobID (Just Delete)) ->
                    msgToSession
                        (Update.UpdateScan <|
                            Update.DeleteScanJob jobID
                        )

                Just (Auto jobID Nothing) ->
                    msgToSession
                        (Update.UpdateConstellation <|
                            Update.GetAutoResults jobID
                        )

                Just (Auto jobID (Just CopyToClipboard)) ->
                    msgToSession
                        (Url.toString url
                            |> String.split "?"
                            |> List.head
                            |> Maybe.withDefault ""
                            |> Update.CopyToClipboard
                        )

                Just (Auto jobID (Just Delete)) ->
                    msgToSession
                        (Update.UpdateConstellation <|
                            Update.DeleteAutoJob jobID
                        )

                Just (Residues jobID Nothing) ->
                    msgToSession
                        (Update.UpdateConstellation <|
                            Update.GetResiduesResults jobID
                        )

                Just (Residues jobID (Just CopyToClipboard)) ->
                    msgToSession
                        (Url.toString url
                            |> String.split "?"
                            |> List.head
                            |> Maybe.withDefault ""
                            |> Update.CopyToClipboard
                        )

                Just (Residues jobID (Just Delete)) ->
                    msgToSession
                        (Update.UpdateConstellation <|
                            Update.DeleteResiduesJob jobID
                        )

                Just (Manual jobID Nothing) ->
                    msgToSession
                        (Update.UpdateConstellation <|
                            Update.GetManualResults jobID
                        )

                Just (Manual jobID (Just CopyToClipboard)) ->
                    msgToSession
                        (Url.toString url
                            |> String.split "?"
                            |> List.head
                            |> Maybe.withDefault ""
                            |> Update.CopyToClipboard
                        )

                Just (Manual jobID (Just Delete)) ->
                    msgToSession
                        (Update.UpdateConstellation <|
                            Update.DeleteManualJob jobID
                        )

                Just (GetResults _) ->
                    ( session
                    , Nav.load <| Url.toString url
                    )

                Nothing ->
                    ( session, Cmd.none )

        TutorialMode _ _ ->
            ( session, Cmd.none )


modelToActiveSession : Session -> Model.Model -> Update.Msg -> ( Session, Cmd Msg )
modelToActiveSession session model msg =
    let
        ( updatedModel, cmds ) =
            Update.update
                msg
                model
    in
    ( { session | mode = ActiveMode updatedModel }
    , Cmd.map ActiveMessage cmds
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
