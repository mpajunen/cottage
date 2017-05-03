module Update exposing (..)

import Array
import Dict
import Random
import Random.Array
import Model exposing (..)


-- ACTIONS


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Draw count ->
            updateGame model msg

        EndTurn ->
            updateGame model msg

        InitGame cards ->
            updateGame model msg

        PlayCard position ->
            updateGame model msg

        SelectCard id ->
            updateGame model msg

        StartGame ->
            let
                { rules, cards } =
                    model

                deckGenerator =
                    createDeck rules.deck cards
            in
                ( { model | game = emptyGame }
                , Random.generate InitGame deckGenerator
                )

        NoOp ->
            ( model, Cmd.none )


updateGame : Model -> Msg -> ( Model, Cmd Msg )
updateGame model msg =
    let
        game =
            playGame model msg
                |> addGameMessage msg
    in
        ( { model | game = game }, Cmd.none )


addGameMessage : Msg -> Game -> Game
addGameMessage msg game =
    { game | messages = game.messages ++ [ msg ] }


playGame : Model -> Msg -> Game
playGame model msg =
    let
        { game, rules } =
            model
    in
        case msg of
            Draw count ->
                draw count game

            EndTurn ->
                endTurn game
                    |> draw rules.roundDraw

            PlayCard position ->
                case game.activeCard of
                    Just id ->
                        playCard game id position

                    Nothing ->
                        game

            SelectCard id ->
                { game | activeCard = Just id }

            InitGame deck ->
                initializeGame model deck

            _ ->
                game


initializeGame : Model -> List GameCard -> Game
initializeGame { game, rules } cards =
    let
        getCardPair { id, card } =
            ( id, card )

        cardMap =
            cards
                |> List.map getCardPair
                |> Dict.fromList

        initialDraw =
            draw rules.initialDraw
    in
        { game
            | cards = cardMap
            , deck = List.map .id cards
        }
            |> initialDraw


createDeck : DeckRules -> Cards -> Random.Generator (List GameCard)
createDeck { cardCount } cards =
    let
        pick =
            pickRandomCard cards
                |> Random.map .id
    in
        Random.list cardCount pick
            |> Random.map (List.indexedMap GameCard)


draw : Int -> Game -> Game
draw count game =
    let
        { turns } =
            game

        turn =
            turns.current

        draws =
            List.take count game.deck

        newTurn =
            { turn | draws = turn.draws ++ draws }
    in
        { game
            | deck = List.drop count game.deck
            , hand = game.hand ++ draws
            , turns = { turns | current = newTurn }
        }


endTurn : Game -> Game
endTurn game =
    let
        { turns } =
            game

        turn =
            turns.current

        newTurns =
            { turns
                | current =
                    { draws = []
                    , plays = []
                    , round = turn.round + 1
                    }
                , history = turns.history ++ [ turn ]
            }
    in
        { game | turns = newTurns }


playCard : Game -> PieceId -> Position -> Game
playCard game id position =
    let
        { board, hand, turns } =
            game

        { current } =
            turns

        ( cards, newHand ) =
            List.partition (\card -> card == id) hand

        newBoard =
            List.foldl (Dict.insert position) board cards

        newPlay =
            { card = id, position = position }

        plays =
            current.plays ++ [ newPlay ]

        newTurns =
            { turns | current = { current | plays = plays } }
    in
        { game
            | board = newBoard
            , hand = newHand
            , turns = newTurns
        }


pickRandomCard : Cards -> Random.Generator Card
pickRandomCard cards =
    cards
        |> Array.fromList
        |> Random.Array.sample
        |> Random.map (Maybe.withDefault invalidCard)
