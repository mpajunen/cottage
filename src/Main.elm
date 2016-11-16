module Main exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)


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


type alias DeckRules =
    { cardCount : CardCount
    }


type alias Rules =
    { deck : DeckRules
    }


type alias Model =
    { cards : Cards
    , deck : Deck
    , rules : Rules
    }


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


initialModel : Model
initialModel =
    { cards = someCards
    , deck = []
    , rules = rules
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp
    | StartGame


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame ->
            let
                { rules, cards } =
                    model

                deck =
                    createDeck rules.deck cards
            in
                ( { model | deck = deck }
                , Cmd.none
                )

        NoOp ->
            ( model, Cmd.none )


createDeck : DeckRules -> Cards -> Deck
createDeck { cardCount } cards =
    let
        { id } =
            List.head cards
                |> Maybe.withDefault invalidCard
    in
        List.repeat cardCount id
            |> List.indexedMap GameCard



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view { cards, deck } =
    div []
        [ h1 [] [ text "Cottage" ]
        , button [ onClick StartGame ] [ text "Start game" ]
        , h2 [] [ text "Cards" ]
        , cardList cards
        , h2 [] [ text "Deck" ]
        , cardList (findDeckCards cards deck)
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
