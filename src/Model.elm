module Model exposing (..)

import AllDict
import Data.Cards exposing (allCards)
import Data.Common exposing (..)


-- MODEL STRUCTURE


type alias Model =
    { cards : Cards
    , game : Game
    , rules : Rules
    }


type Msg
    = NoOp
    | EndTurn
    | PlayCard Position
    | SelectCard PieceId
    | StartGame
    | InitGame (List GameCard)



-- INITIAL MODEL


ordPiece : PieceId -> Int
ordPiece (PieceId id) =
    id


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
    { cards = allCards
    , game = emptyGame
    , rules = rules
    }


invalidPieceId : PieceId
invalidPieceId =
    PieceId -1
