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
    List ( PieceId, Card )


type alias Deck =
    List GameCard


type alias Hand =
    List GameCard


type alias Coordinate =
    Int


type alias Position =
    ( Coordinate, Coordinate )


type alias BoardCards =
    Dict.Dict Position GameCard


type alias BoardCell =
    { position : Position
    , card : Maybe GameCard
    }


type alias BoardRow =
    List BoardCell


type alias Board =
    List BoardRow


type alias Messages =
    List Msg


type alias Game =
    { activeCard : Maybe PieceId
    , board : BoardCards
    , deck : Deck
    , hand : Hand
    , messages : Messages
    }


type alias DeckRules =
    { cardCount : CardCount
    }


type alias Rules =
    { deck : DeckRules
    , initialDraw : CardCount
    }


type Msg
    = NoOp
    | Draw Int
    | PlayCard Position
    | SelectCard PieceId
    | StartGame
    | InitGame Deck


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
    }


emptyBoard : BoardCards
emptyBoard =
    Dict.empty


emptyGame : Game
emptyGame =
    { activeCard = Nothing
    , board = emptyBoard
    , deck = []
    , hand = []
    , messages = []
    }


initialModel : Model
initialModel =
    { cards = someCards
    , game = emptyGame
    , rules = rules
    }
