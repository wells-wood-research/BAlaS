module Fancy exposing
    ( button
    , buttonInputStyle
    , colourPalette
    , controlButton
    , details
    , h1
    , h2
    , h3
    , h4
    , input
    , p
    , smallHeaderStyle
    , table
    , td
    , th
    , tr
    )

import Css
import Html
import Html.Styled as Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (onClick)



-- https://coolors.co/ffffff-412234-6d466b-b49fcc-ead7d7


colourPalette :
    { c1 : Css.Color
    , c2 : Css.Color
    , c3 : Css.Color
    , c4 : Css.Color
    , c5 : Css.Color
    , white : Css.Color
    }
colourPalette =
    { c1 = Css.hex "678d58"
    , c2 = Css.hex "9ad5ca"
    , c3 = Css.hex "7579af"
    , c4 = Css.hex "b49fcc"
    , c5 = Css.hex "ead7d7"
    , white = Css.hex "ffffff"
    , red = Css.hex 
    }



-- Redefined Elements


h1 : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
h1 =
    styled Styled.h1
        [ Css.color colourPalette.c1
        , Css.fontFamilies [ "IBM Plex Sans Condensed", "sans-serif" ]
        , Css.margin Css.zero
        ]


smallHeaderStyle : Css.Style
smallHeaderStyle =
    Css.batch
        [ Css.margin Css.auto
        , Css.padding (Css.px 3)
        ]


h2 : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
h2 =
    styled Styled.h2
        [ smallHeaderStyle ]


h3 : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
h3 =
    styled Styled.h3
        [ smallHeaderStyle ]


h4 : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
h4 =
    styled Styled.h4
        [ smallHeaderStyle ]


p : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
p =
    styled Styled.p
        [ Css.margin Css.auto
        , Css.padding (Css.px 3)
        ]


details : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
details =
    styled Styled.details
        [ Css.fontSize (Css.pt 10)
        ]


buttonInputStyle : Css.Style
buttonInputStyle =
    Css.batch
        [ Css.borderColor <| Css.hex "000000"
        , Css.borderStyle Css.solid
        , Css.borderWidth (Css.px 1)
        , Css.color <| Css.hex "000000"
        , Css.margin (Css.px 3)
        , Css.padding (Css.px 3)
        ]


button : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
button =
    styled Styled.button
        [ buttonInputStyle
        , Css.backgroundColor <| Css.hex "aaaaaa"
        , Css.active
            [ Css.backgroundColor <| Css.hex "888888" ]
        , Css.disabled
            [ Css.backgroundColor <| Css.hex "bbbbbb"
            , Css.color <| Css.hex "999999"
            ]
        , Css.hover
            [ Css.backgroundColor <| Css.hex "999999" ]
        ]


input : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
input =
    styled Styled.input
        [ buttonInputStyle
        ]


table : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
table =
    styled Styled.table
        [ Css.backgroundColor <| Css.hex "ffffff"
        , Css.borderCollapse Css.collapse
        , Css.margin4 (Css.pct 1) (Css.pct 0) (Css.pct 1) (Css.pct 0)
        , Css.tableLayout Css.fixed
        , Css.width (Css.pct 100)
        ]


td : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
td =
    styled Styled.td
        [ Css.textAlign Css.left
        , Css.padding (Css.px 8)
        , Css.overflowWrap Css.breakWord
        ]


th : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
th =
    styled Styled.th
        [ Css.textAlign Css.left
        , Css.padding (Css.px 8)
        ]


tr : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
tr =
    styled Styled.tr
        [ Css.fontSize (Css.pt 10)
        , Css.nthChild
            "even"
            [ Css.backgroundColor <| Css.hex "dddddd"
            ]
        , Css.hover
            [ Css.backgroundColor <| Css.hex "cccccc"
            ]
        ]



-- Shared


controlButton : List (Styled.Attribute msg) -> List (Styled.Html msg) -> Styled.Html msg
controlButton =
    styled button
        [ Css.backgroundColor colourPalette.c2
        , Css.borderStyle Css.solid
        , Css.borderWidth (Css.px 1)
        , Css.cursor Css.pointer
        , Css.margin (Css.px 5)
        , Css.padding (Css.px 3)
        , Css.textAlign Css.center
        , Css.width (Css.px 40)
        ]



{-
   @media (max-width: 1080px) {
       .main-grid {
           grid-template-columns: 100%;
           grid-template-rows: 5% 50% 5% 40%;
       }

       .control-panel {
           grid-column: 1 / 2;
           grid-row: 4 / 5;
       }

       .tabs {
           grid-column: 1 / 2;
           grid-row: 3 / 4;
       }
   }
-}
