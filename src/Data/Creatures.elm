module Data.Creatures exposing (..)

import Data.Common exposing (..)


footman : Creature
footman =
    { name = "Footman"
    , attack = 3
    , defense = 1
    , life = 3
    }


golem : Creature
golem =
    { name = "Golem"
    , attack = 5
    , defense = 3
    , life = 12
    }


knight : Creature
knight =
    { name = "Knight"
    , attack = 6
    , defense = 2
    , life = 8
    }
