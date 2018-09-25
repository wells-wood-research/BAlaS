module Fancy exposing
    ( button
    , buttonInputStyle
    , colourPalette
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
    }


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
        , Css.hover
            [ Css.backgroundColor <| Css.hex "999999" ]
        , Css.active
            [ Css.backgroundColor <| Css.hex "888888" ]
        , Css.disabled
            [ Css.backgroundColor <| Css.hex "bbbbbb"
            , Css.color <| Css.hex "999999"
            ]
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



{-
   body {
       font-family: 'Titillium Web', sans-serif;
   }

   .details {
       font-size: 10pt;
   }


   #viewer {
       grid-column: 1 / 2;
       grid-row: 2 / 3;
       overflow: hidden;
       position: relative;
   }

   .hover-label {
       background-color: white;
       padding: 2px;
       position: absolute;
       z-index:100;
   }

   .banner {
       align-items: center;
       background-color: var(--c2);
       display: flex;
       grid-column: 1 / 2;
       grid-row: 1 / 2;
       justify-content: space-between;
       padding: 5px;
   }

   .controls {
       display: flex;
   }

   .notifications {
       text-align: right;
   }

   .control-button {
       background-color: var(--c2);
       border-style: solid;
       border-width: 1px;
       cursor: pointer;
       margin: 5px;
       padding: 3px;
       text-align: center;
       width: 40px;
   }

   .notification-panel {
       background-color: var(--c2);
       box-shadow: 0 5px 10px 0 rgba(133,133,133,1);
       height: 100%;
       left: 0;
       position: fixed;
       text-align: center;
       top: 0;
       width: 300px;
       z-index:1000;
   }

   .notification {
       background: var(--c3);
       margin: 3px;
       padding: 3px;
       text-align: left;
   }

   .notification > .details {
       background: var(--c4);
       margin: 3px;
       padding: 3px;
   }

   .tabs {
       display: grid;
       grid-template-columns: repeat(3, 1fr);
       grid-column: 2 / 3;
       grid-row: 1 / 2;
       text-align: center;
   }

   .tabs:hover {
       cursor: pointer;
   }

   .tab {
       align-items: center;
       display: flex;
       justify-content: center;
   }

   .scan-tab {
       background-color: var(--c3);
   }

   .constellation-tab {
       background-color: var(--c4);
   }

   .jobs-tab {
       background-color: var(--c5);
   }

   .control-panel {
       grid-column: 2 / 3;
       grid-row: 2 / 3;
       padding: 10px;
       overflow: auto;
       max-width: 100%;
   }

   .scan-panel {
       background-color: var(--c3);
   }

   .constellation-panel {
       background-color: var(--c4);
   }

   .jobs-panel {
       background-color: var(--c5);
   }

   .scan-results-table {
       table-layout: fixed;
       width: 100%;
   }

   .selected-residue {
       background-color: red !important;
   }

   .jobs-table {
       table-layout: fixed;
       width: 100%;
   }

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
