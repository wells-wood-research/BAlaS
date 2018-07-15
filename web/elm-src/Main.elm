port module Main exposing (..)

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

import Html exposing (..)
import Model
import Ports
import Time
import Update
import View


main : Program (Maybe Model.ExportableModel) Model.Model Update.Msg
main =
    programWithFlags
        { init = init
        , view = View.view
        , update = Update.update
        , subscriptions = subscriptions
        }


{-| Creates a model and Cmd Msgs for the initial state of the application.
-}
init : Maybe Model.ExportableModel -> ( Model.Model, Cmd msg )
init mExported =
    case mExported of
        Just exported ->
            Model.importModel exported ! [ Ports.initialiseViewer () ]

        Nothing ->
            Model.emptyModel ! [ Ports.initialiseViewer () ]


{-| Describes the active subscriptions based on the current state of the model.
-}
subscriptions : Model.Model -> Sub Update.Msg
subscriptions model =
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
                    [ Time.every (5 * Time.second)
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
                    [ Time.every (5 * Time.second)
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
                    [ Time.every (5 * Time.second)
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
                    [ Time.every (5 * Time.second)
                        (Update.UpdateConstellation << Update.CheckResiduesJobs)
                    ]
                else
                    []
               )
