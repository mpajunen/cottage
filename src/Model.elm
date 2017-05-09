module Model exposing (..)

import AllDict


-- MODEL STRUCTURE


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


type Msg
    = NoOp
    | EndTurn
    | PlayCard Position
    | SelectCard PieceId
    | StartGame
    | InitGame (List GameCard)


type alias Model =
    { cards : Cards
    , game : Game
    , rules : Rules
    }



-- INITIAL MODEL


ordPiece : PieceId -> Int
ordPiece (PieceId id) =
    id


someCards : Cards
someCards =
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
    ]


invalidCardId : CardId
invalidCardId =
    CardId -1


invalidCard : Card
invalidCard =
    { id = invalidCardId
    , name = "Invalid card"
    , cost = []
    , effects = []
    }


rules : Rules
rules =
    { deck =
        { cardCount = 30
        }
    , resources =
        [ { resource = Build, value = 3, roundGain = 1 }
        , { resource = Command, value = 3, roundGain = 0 }
        , { resource = Magic, value = 3, roundGain = 0 }
        ]
    , initialDraw = 5
    , roundDraw = 3
    }


firstTurn : Turn
firstTurn =
    { draws = []
    , plays = []
    , round = 1
    }


initialTurns : Turns
initialTurns =
    { current = firstTurn
    , history = []
    }


emptyGame : Game
emptyGame =
    { activeCard = Nothing
    , board = []
    , cards = AllDict.empty ordPiece
    , deck = []
    , hand = []
    , resources = []
    , turns = initialTurns
    }


initialModel : Model
initialModel =
    { cards = someCards
    , game = emptyGame
    , rules = rules
    }


invalidPieceId : PieceId
invalidPieceId =
    PieceId -1
