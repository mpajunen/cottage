module Update exposing (..)

import Array
import Dict
import Random
import Random.Array
import Model exposing (..)


-- MESSAGE STRUCTURE


type Msg
    = NoOp
    | Draw Int
    | PlayCard Position
    | SelectCard PieceId
    | StartGame
    | ReceiveDeck Deck



-- ACTIONS


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw count ->
            ( { model | player = draw count model.player }, Cmd.none )

        PlayCard position ->
            let
                newModel =
                    case model.ui.activeCard of
                        Just id ->
                            playCard model id position

                        Nothing ->
                            model
            in
                ( newModel, Cmd.none )

        SelectCard id ->
            let
                { ui } =
                    model

                updatedUi =
                    { ui | activeCard = Just id }
            in
                ( { model | ui = updatedUi }, Cmd.none )

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
                ( { model
                    | board = emptyBoard
                    , player = player
                  }
                , Cmd.none
                )

        NoOp ->
            ( model, Cmd.none )


initializePlayer : Model -> Deck -> Player
initializePlayer { player, rules } deck =
    let
        initialDraw =
            draw rules.initialDraw
    in
        { player | deck = deck }
            |> initialDraw


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


playCard : Model -> PieceId -> Position -> Model
playCard model id position =
    let
        { board, player } =
            model

        ( cards, newHand ) =
            List.partition (\card -> card.id == id) player.hand

        newBoard =
            List.foldl (Dict.insert position) board cards

        newPlayer =
            { player | hand = newHand }
    in
        { model
            | board = newBoard
            , player = newPlayer
        }


pickRandomCard : Cards -> Random.Generator Card
pickRandomCard cards =
    cards
        |> Array.fromList
        |> Random.Array.sample
        |> Random.map (Maybe.withDefault invalidCard)
