module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Array
import Random
import Random.Array


--import Html.Attributes exposing (..)
--import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


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


type alias Deck =
    List GameCard


type alias Hand =
    List GameCard


type alias Player =
    { deck : Deck
    , hand : Hand
    }


type alias DeckRules =
    { cardCount : CardCount
    }


type alias Rules =
    { deck : DeckRules
    }


type alias Model =
    { cards : Cards
    , player : Player
    , rules : Rules
    }


type alias Style =
    List ( String, String )


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
    }


newPlayer : Player
newPlayer =
    { deck = []
    , hand = []
    }


initialModel : Model
initialModel =
    { cards = someCards
    , player = newPlayer
    , rules = rules
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | Draw Int
    | StartGame
    | ReceiveDeck Deck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw count ->
            ( { model | player = draw count model.player }, Cmd.none )

        StartGame ->
            let
                { rules, cards } =
                    model

                deckGenerator =
                    createDeck rules.deck cards
            in
                ( { model | player = newPlayer }
                , Random.generate ReceiveDeck deckGenerator
                )

        ReceiveDeck deck ->
            let
                player =
                    initializePlayer model deck
            in
                ( { model | player = player }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )


initializePlayer : Model -> Deck -> Player
initializePlayer { player } deck =
    { player | deck = deck }


createDeck : DeckRules -> Cards -> Random.Generator Deck
createDeck { cardCount } cards =
    let
        pick =
            pickRandomCard cards
                |> Random.map (\card -> card.id)
    in
        Random.list cardCount pick
            |> Random.map (List.indexedMap GameCard)


draw : Int -> Player -> Player
draw count player =
    { player
        | deck = List.drop count player.deck
        , hand = player.hand ++ (List.take count player.deck)
    }


pickRandomCard : Cards -> Random.Generator Card
pickRandomCard cards =
    cards
        |> Array.fromList
        |> Random.Array.sample
        |> Random.map (Maybe.withDefault invalidCard)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Cottage" ]
        , button [ onClick StartGame ] [ text "Start game" ]
        , div [ style mainStyle ]
            [ gameView model
            , cardsView model.cards
            ]
        ]


gameView : Model -> Html Msg
gameView { cards, player } =
    div [ style gameStyle ]
        [ h2 [] [ text "Hand" ]
        , cardBox (findDeckCards cards player.hand)
        , h2 [] [ text "Deck" ]
        , button [ onClick (Draw 1) ] [ text "Draw" ]
        , deckView player.deck
        , cardList (findDeckCards cards player.deck)
        ]


deckView : Deck -> Html Msg
deckView deck =
    div [ style deckStyle ]
        [ text ("(" ++ (toString <| List.length deck) ++ ")") ]


cardsView : Cards -> Html Msg
cardsView cards =
    div [ style cardsStyle ]
        [ h2 [] [ text "Cards" ]
        , cardBox cards
        ]


cardBox : Cards -> Html Msg
cardBox cards =
    div [ style cardBoxStyle ]
        (List.map cardView cards)


cardView : Card -> Html Msg
cardView card =
    div [ style cardStyle ]
        [ cardBar card
        ]


cardBar : Card -> Html Msg
cardBar card =
    div [ style cardBarStyle ]
        [ div [ style cardTitleStyle ] [ text card.name ]
        , div [ style cardCostStyle ] [ text <| toString card.cost ]
        ]


cardList : Cards -> Html Msg
cardList cards =
    table []
        [ thead []
            [ tableRow True cardFieldLabels ]
        , tbody []
            (List.map cardRow cards)
        ]


cardRow : Card -> Html Msg
cardRow card =
    card
        |> getCardValues
        |> tableRow False


tableRow : Bool -> List String -> Html Msg
tableRow header values =
    tr []
        (List.map (tableCell header) values)


tableCell : Bool -> String -> Html Msg
tableCell header value =
    case header of
        False ->
            td [] [ text value ]

        True ->
            th [] [ text value ]


findDeckCards : Cards -> Deck -> Cards
findDeckCards cards deck =
    deck
        |> List.map (\card -> card.card)
        |> List.map (findCard cards)


findCard : Cards -> CardId -> Card
findCard cards id =
    let
        card =
            cards
                |> List.filter (\card -> card.id == id)
                |> List.head
    in
        Maybe.withDefault invalidCard card


getCardValues : Card -> List String
getCardValues { name, cost } =
    [ name
    , toString cost
    ]


cardFieldLabels : List String
cardFieldLabels =
    [ "Name"
    , "Cost"
    ]


mainStyle : Style
mainStyle =
    [ ( "display", "flex" )
    ]


gameStyle : Style
gameStyle =
    [ ( "flex", "3 auto" )
    ]


cardsStyle : Style
cardsStyle =
    [ ( "flex", "1 auto" )
    ]


cardBoxStyle : Style
cardBoxStyle =
    [ ( "display", "flex" )
    , ( "justify-content", "center" )
    , ( "flex-flow", "row wrap" )
    ]


deckStyle : Style
deckStyle =
    cardStyle
        ++ [ ( "display", "flex" )
           , ( "background-color", "#dddddd" )
           , ( "flex-flow", "column" )
           , ( "align-items", "center" )
           , ( "justify-content", "center" )
           ]


cardStyle : Style
cardStyle =
    [ ( "border", "1px solid #000000" )
    , ( "height", "180px" )
    , ( "width", "120px" )
    , ( "margin", "5px" )
    ]


cardBarStyle : Style
cardBarStyle =
    [ ( "display", "flex" )
    ]


cardCostStyle : Style
cardCostStyle =
    [ ( "flex", "1 auto" )
    , ( "padding", "5px" )
    ]


cardTitleStyle : Style
cardTitleStyle =
    [ ( "flex", "3 auto" )
    , ( "padding", "5px" )
    ]
