port module Notifications exposing (Notification, emptyNotification, encodeNotification, errorToNotification, notificationDecoder)

{-| Module containing the notifications data structure and helper functions.
-}

import Http
import Json.Decode exposing (bool, decodeString, field, float, string)
import Json.Decode.Extra exposing ((|:))
import Json.Encode as JEn


type alias Notification =
    { date : String
    , title : String
    , message : String
    }


(#) : ( a, b ) -> List Notification -> ( a, b, List Notification )
(#) ( a, b ) notifications =
    ( a, b, notifications )


emptyNotification : Notification
emptyNotification =
    Notification
        ""
        ""
        ""


notificationDecoder : Json.Decode.Decoder Notification
notificationDecoder =
    Json.Decode.succeed Notification
        |: field "date" string
        |: field "title" string
        |: field "message" string


encodeNotification : Notification -> JEn.Value
encodeNotification notification =
    JEn.object
        [ ( "date", JEn.string notification.date )
        , ( "title", JEn.string notification.title )
        , ( "message", JEn.string notification.message )
        ]


errorToNotification : String -> Http.Error -> Notification
errorToNotification title err =
    Notification
        ""
        title
        (toString err)
