module Data.Cards exposing (allCards)

import Data.Common exposing (..)


allCards : Cards
allCards =
    [ { id = CardId 1
      , name = "Barracks"
      , cost = [ ( Build, 2 ) ]
      , effects =
            [ (Gain ( Command, 1 ))
            ]
      }
    , { id = CardId 2
      , name = "Laboratory"
      , cost = [ ( Build, 3 ) ]
      , effects =
            [ (Gain ( Magic, 1 ))
            ]
      }
    , { id = CardId 3
      , name = "Cavern"
      , cost = [ ( Build, 2 ) ]
      , effects =
            [ (Gain ( Command, 2 ))
            ]
      }
    , { id = CardId 4
      , name = "Quarry"
      , cost = [ ( Build, 1 ) ]
      , effects =
            [ (Gain ( Build, 1 ))
            ]
      }
    , { id = CardId 5
      , name = "Mine"
      , cost = [ ( Build, 3 ) ]
      , effects =
            [ (Gain ( Build, 2 ))
            ]
      }
    , { id = CardId 6
      , name = "Experimental laboratory"
      , cost = [ ( Build, 5 ) ]
      , effects =
            [ (Gain ( Magic, 3 ))
            , (Gain ( Command, -1 ))
            ]
      }
    , { id = CardId 7
      , name = "Footman"
      , cost = [ ( Command, 1 ) ]
      , effects =
            [ (Summon (Creature 3 1 3))
            ]
      }
    , { id = CardId 8
      , name = "Knight"
      , cost = [ ( Command, 3 ) ]
      , effects =
            [ (Summon (Creature 6 2 8))
            , (Gain ( Command, -1 ))
            ]
      }
    , { id = CardId 9
      , name = "Golem"
      , cost = [ ( Build, 1 ), ( Command, 1 ), ( Magic, 2 ) ]
      , effects =
            [ (Summon (Creature 5 3 12))
            , (Gain ( Magic, -1 ))
            ]
      }
    ]
