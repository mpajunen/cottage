module View.Styles exposing (..)

import Css exposing (..)
import Html
import Html.Attributes


-- UTILS


styles : List Mixin -> Html.Attribute msg
styles =
    Css.asPairs >> Html.Attributes.style



-- ELEMENT STYLES


card : List Mixin
card =
    [ border3 (px 1) solid (hex "000000")
    , height (px 180)
    , width (px 120)
    , margin (px 5)
    ]


cardBar : List Mixin
cardBar =
    [ displayFlex ]


cardBox : List Mixin
cardBox =
    [ displayFlex
    , justifyContent center
    , flexFlow2 wrap row
    ]


cardCost : List Mixin
cardCost =
    [ flex (int 1)
    , padding (px 5)
    ]


cards : List Mixin
cards =
    [ flex (int 1) ]


cardTitle : List Mixin
cardTitle =
    [ flex (int 3)
    , padding (px 5)
    ]


content : List Mixin
content =
    [ displayFlex ]


deck : List Mixin
deck =
    card
        ++ [ displayFlex
           , backgroundColor (hex "dddddd")
           , flexFlow1 column
           , alignItems center
           , justifyContent center
           ]


effect : List Mixin
effect =
    [ padding (px 5)
    ]


game : List Mixin
game =
    [ flex (int 3) ]


selected : List Mixin
selected =
    [ backgroundColor (hex "aaaaaa") ]


slot : List Mixin
slot =
    [ border3 (px 1) solid (hex "555555")
    , height (px 60)
    , width (px 120)
    , margin (px 5)
    ]


turns : List Mixin
turns =
    cards


unavailable : List Mixin
unavailable =
    [ borderColor (hex "aaaaaa")
    , color (hex "aaaaaa")
    ]


underground : List Mixin
underground =
    [ backgroundColor (hex "888888") ]
