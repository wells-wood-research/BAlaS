module Session exposing (Msg(..), Session(..), update, view)

import Html exposing (Html)
import Model
import Tutorial
import Update
import View


type Session
    = ActiveMode Model.Model Bool
    | TutorialMode (Tutorial.Tutorial Update.Msg)


type Msg
    = ToggleMode
    | ActiveMsg Update.Msg


update : Msg -> Session -> ( Session, Cmd Msg )
update msg session =
    case msg of
        ActiveMsg activeMsg ->
            case session of
                ActiveMode model _ ->
                    let
                        ( updatedModel, cmds ) =
                            Update.update activeMsg model
                    in
                    ( ActiveMode updatedModel True
                    , Cmd.map ActiveMsg cmds
                    )

                TutorialMode _ ->
                    ( session, Cmd.none )

        ToggleMode ->
            ( session, Cmd.none )


view : Session -> Html Msg
view session =
    case session of
        ActiveMode model _ ->
            Html.map ActiveMsg <|
                View.view model

        TutorialMode (Tutorial.Tutorial _ section _) ->
            Html.map ActiveMsg <|
                View.view section.model
