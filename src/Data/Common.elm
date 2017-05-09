module Data.Common exposing (..)

import AllDict


type Resource
    = Build
    | Command
    | Magic


type alias ResourceCount =
    Int


type alias ResourceAmount =
    ( Resource, ResourceCount )


type alias Resources =
    List ResourceAmount


type alias Life =
    Int


type alias Creature =
    { attack : Life
    , defense : Life
    , life : Life
    }


type Effect
    = Gain ResourceAmount
    | Summon Creature


type alias Effects =
    List Effect


type CardId
    = CardId Int


type PieceId
    = PieceId Int


type alias Card =
    { id : CardId
    , name : String
    , cost : Resources
    , effects : Effects
    }


type alias Cards =
    List Card


type alias CardCount =
    Int


type alias GameCard =
    { id : PieceId
    , card : CardId
    }


type alias GameCards =
    AllDict.AllDict PieceId CardId Int


type alias Deck =
    List PieceId


type alias Hand =
    List PieceId


type alias Coordinate =
    Int


type alias Position =
    { x : Coordinate
    , y : Coordinate
    }


type alias Play =
    { card : PieceId
    , position : Position
    }


type alias RoundNumber =
    Int


type alias Turn =
    { draws : List PieceId
    , plays : List Play
    , round : RoundNumber
    }


type alias TurnHistory =
    List Turn


type alias Turns =
    { current : Turn
    , history : TurnHistory
    }


type alias Game =
    { activeCard : Maybe PieceId
    , cards : GameCards
    , board : List Play
    , deck : Deck
    , hand : Hand
    , resources : Resources
    , turns : Turns
    }


type alias ResourceInfo =
    { resource : Resource
    , value : Int
    , roundGain : Int
    }


type alias DeckRules =
    { cardCount : CardCount
    }


type alias Rules =
    { deck : DeckRules
    , resources : List ResourceInfo
    , initialDraw : CardCount
    , roundDraw : CardCount
    }


type alias Model =
    { cards : Cards
    , game : Game
    , rules : Rules
    }
