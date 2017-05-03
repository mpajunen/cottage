module Model exposing (..)

import Dict


-- MODEL STRUCTURE


type alias CardId =
    Int


type alias Cost =
    Int


type alias PieceId =
    Int


type alias Card =
    { id : CardId
    , name : String
    , cost : Cost
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
    Dict.Dict PieceId CardId


type alias Deck =
    List PieceId


type alias Hand =
    List PieceId


type alias Coordinate =
    Int


type alias Position =
    ( Coordinate, Coordinate )


type alias BoardCards =
    Dict.Dict Position PieceId


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


type alias Messages =
    List Msg


type alias Game =
    { activeCard : Maybe PieceId
    , cards : GameCards
    , board : BoardCards
    , deck : Deck
    , hand : Hand
    , messages : Messages
    , turns : Turns
    }


type alias DeckRules =
    { cardCount : CardCount
    }


type alias Rules =
    { deck : DeckRules
    , initialDraw : CardCount
    , roundDraw : CardCount
    }


type Msg
    = NoOp
    | Draw Int
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


someCards : Cards
someCards =
    [ { id = 1
      , name = "Barracks"
      , cost = 2
      }
    , { id = 2
      , name = "Laboratory"
      , cost = 3
      }
    , { id = 3
      , name = "Cavern"
      , cost = 2
      }
    ]


invalidCard : Card
invalidCard =
    { id = -1
    , name = "Invalid card"
    , cost = 0
    }


rules : Rules
rules =
    { deck =
        { cardCount = 30
        }
    , initialDraw = 5
    , roundDraw = 3
    }


emptyBoard : BoardCards
emptyBoard =
    Dict.empty


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
    , board = emptyBoard
    , cards = Dict.empty
    , deck = []
    , hand = []
    , messages = []
    , turns = initialTurns
    }


initialModel : Model
initialModel =
    { cards = someCards
    , game = emptyGame
    , rules = rules
    }
