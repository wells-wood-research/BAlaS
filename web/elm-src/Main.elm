module Main exposing (init, main, subscriptions)

{-| This module provides the main entry point to the BALS web application
front end.

The application is composed of 4 modules:

1.  **Model.elm** - Contains the types that describe the state of the
    application, as well functions creating, validating and encode/decoding them
    to/from JSON.
2.  **Update.elm** - The update functions and message types used to update the
    state of the model based on user input.
3.  **Ports.elm** - Handles communication with the world outside the Elm
    application, including JavaScript inter opt and HTTP requests.
4.  **View.elm** - All the functions that describe how the current state of the
    application could be rendered in the browser.

-}

import Browser
import Css
import Html
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Json.Decode as JDe
import Model exposing (emptyModel)
import Notifications
import Ports
import Session
import Time
import Update
import View


main : Program JDe.Value Session.Session Session.Msg
main =
    Browser.element
        { init = init
        , view = Session.view >> Styled.toUnstyled
        , update = Session.update
        , subscriptions = subscriptions
        }


{-| Creates a model and Cmd Msgs for the initial state of the application.
-}
init : JDe.Value -> ( Session.Session, Cmd Session.Msg )
init saveState =
    case Model.loadModel saveState of
        Ok model ->
            ( Session.ActiveMode model True
            , Ports.initialiseViewer ()
            )

        Err error ->
            ( Session.ActiveMode
                { emptyModel
                    | notifications =
                        [ Notifications.Notification
                            ""
                            "Something went wrong while loading your last session..."
                            (JDe.errorToString error)
                        ]
                }
                True
            , Ports.initialiseViewer ()
            )


{-| Describes the active subscriptions based on the current state of the model.
-}
subscriptions : Session.Session -> Sub Session.Msg
subscriptions session =
    case session of
        Session.ActiveMode model _ ->
            Sub.map Session.ActiveMessage <|
                appSubscriptions model

        Session.TutorialMode _ _ ->
            Sub.none


appSubscriptions : Model.Model -> Sub Update.Msg
appSubscriptions model =
    Sub.batch <|
        [ Ports.receiveStructure <| Update.UpdateScan << Update.UpdateStructure

        -- , atomClick <| UpdateScan << ToggleResidueSelection
        ]
            ++ (case model.alanineScan.structure of
                    Just _ ->
                        [ Ports.hoveredName Update.SetHoveredName ]

                    Nothing ->
                        []
               )
            ++ (if
                    List.length
                        (Model.getActiveJobs model.alanineScan.jobs)
                        > 0
                then
                    [ Time.every 5000
                        (Update.UpdateScan << Update.CheckScanJobs)
                    ]

                else
                    []
               )
            ++ (if
                    List.length
                        (Model.getActiveJobs model.constellation.autoJobs)
                        > 0
                then
                    [ Time.every 5000
                        (Update.UpdateConstellation << Update.CheckAutoJobs)
                    ]

                else
                    []
               )
            ++ (if
                    List.length
                        (Model.getActiveJobs model.constellation.manualJobs)
                        > 0
                then
                    [ Time.every 5000
                        (Update.UpdateConstellation << Update.CheckManualJobs)
                    ]

                else
                    []
               )
            ++ (if
                    List.length
                        (Model.getActiveJobs model.constellation.residuesJobs)
                        > 0
                then
                    [ Time.every 5000
                        (Update.UpdateConstellation << Update.CheckResiduesJobs)
                    ]

                else
                    []
               )
