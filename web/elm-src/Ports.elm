port module Ports exposing
    ( atomClick
    , clearViewer
    , colourGeometry
    , colourResidues
    , displayScanResults
    , focusOnResidue
    , hoveredName
    , initialiseViewer
    , receiveStructure
    , requestPDBFile
    , saveState
    , setVisibility
    , showStructure
    )

{- # COMMANDS AND SUBSCRIPTIONS
   This section contains all code for communicating with the outside world, whether
   that's with the server or javascript inside the index.html file.
-}

import Json.Decode as JDe
import Model



{- Each of the following ports corresponds to a JavaScript function in
   index.html. The function is ran when the port command is returned by the update
   function.
-}


port saveState : JDe.Value -> Cmd msg


port initialiseViewer : () -> Cmd msg


port clearViewer : () -> Cmd msg


port showStructure : String -> Cmd msg


port requestPDBFile : () -> Cmd msg


port setVisibility : ( String, Bool ) -> Cmd msg


port colourGeometry : ( String, Model.ChainID ) -> Cmd msg


port colourResidues : Model.ResidueColour -> Cmd msg


port displayScanResults : Model.AlanineScanResults -> Cmd msg


port focusOnResidue : Model.ResidueResult -> Cmd msg



-- Subscriptions
{- The following ports are triggered by JavaScript functions in index.html and
   send data into the Elm application.
-}


port receiveStructure : (Maybe Model.Structure -> msg) -> Sub msg


port hoveredName : (Maybe String -> msg) -> Sub msg


port atomClick : (Model.ResidueInfo -> msg) -> Sub msg
