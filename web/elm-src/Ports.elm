port module Ports exposing (..)

{- # COMMANDS AND SUBSCRIPTIONS
   This section contains all code for communicating with the outside world, whether
   that's with the server or javascript inside the index.html file.
-}

import Model


{- Each of the following ports corresponds to a JavaScript function in
   index.html. The function is ran when the port command is returned by the update
   function.
-}


port saveState : Model.ExportableModel -> Cmd msg


port initialiseViewer : () -> Cmd msg


port requestPDBFile : () -> Cmd msg


port setVisibility : ( String, Bool ) -> Cmd msg


port colourGeometry : ( String, Model.ChainID ) -> Cmd msg


port displayScanResults : Model.AlanineScanResults -> Cmd msg


port focusOnResidue : Model.ResidueResult -> Cmd msg



-- Subscriptions
{- The following ports are triggered by JavaScript functions in index.html and
   send data into the Elm application.
-}


port receiveStructure : (Model.ExportableStructure -> msg) -> Sub msg


port atomClick : (Model.ResidueInfo -> msg) -> Sub msg
