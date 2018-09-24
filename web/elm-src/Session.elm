module Session exposing (Msg(..), Session(..), update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model
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
                    ( TutorialMode <|
                        tutorialUpdate tutorialMsg tutorial
                    , Cmd.none
                    )

                ActiveMode _ _ ->
                    ( session, Cmd.none )

        ToggleMode ->
            case session of
                ActiveMode model _ ->
                    ( { previous = PreviousSection
                      , next = NextSection
                      , cancel = CancelTutorial
                      }
                        |> Tutorial.tutorial
                        |> TutorialMode
                    , Cmd.none
                    )

                TutorialMode _ ->
                    ( ActiveMode Model.emptyModel True
                    , Cmd.none
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


view : Session -> Html Msg
view session =
    div []
        ([ button
            [ onClick ToggleMode
            , style "z-index" "2000"
            , style "position" "absolute"
            ]
            [ text "Toggle" ]
         ]
            ++ [ case session of
                    ActiveMode model _ ->
                        div []
                            [ Html.map ActiveMessage <|
                                View.view model
                            ]

                    TutorialMode (Tutorial.Tutorial _ section _) ->
                        p []
                            [ button [ onClick <| TutorialMessage CancelTutorial ]
                                [ text "Cancel" ]
                            , Html.map ActiveMessage <|
                                View.view section.model
                            ]
               ]
        )
