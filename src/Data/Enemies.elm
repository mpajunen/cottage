module Data.Enemies exposing (..)

import Dict
import Data.Common exposing (Enemies)
import Data.Creatures exposing (..)


enemyWaves : Enemies
enemyWaves =
    Dict.fromList
        [ ( 1, [] )
        , ( 2, [] )
        , ( 3, [ footman ] )
        , ( 4, [ knight ] )
        , ( 5, [ knight, footman ] )
        , ( 6, [ footman, footman, footman, footman ] )
        , ( 7, [ knight, footman, footman, footman ] )
        , ( 8, [ golem, golem ] )
        , ( 9, [ golem, knight, knight, footman, footman ] )
        , ( 10, [ golem, knight, knight, knight, footman, footman, footman ] )
        ]
