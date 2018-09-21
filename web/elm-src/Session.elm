module Session exposing (Session(..), update, view)

import Html exposing (Html)
import Model
import Tutorial
import Update
import View


type Session
    = ActiveMode Model.Model Bool
    | TutorialMode (Tutorial.Tutorial Update.Msg)


update : Update.Msg -> Session -> ( Session, Cmd Update.Msg )
update msg session =
    case session of
        ActiveMode model _ ->
            let
                ( updatedModel, cmds ) =
                    Update.update msg model
            in
            ( ActiveMode updatedModel True
            , cmds
            )

        TutorialMode _ ->
            ( session, Cmd.none )


view : Session -> Html Update.Msg
view session =
    case session of
        ActiveMode model _ ->
            View.view model

        TutorialMode (Tutorial.Tutorial _ section _) ->
            View.view section.model
