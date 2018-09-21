module Session exposing (Session(..))

import Model


type Session
    = ActiveMode Model.Model Bool
