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

        InitGame deck ->
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
        { game } =
            model
    in
        case msg of
            Draw count ->
                draw count game

            EndTurn ->
                endTurn game

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


initializeGame : Model -> Deck -> Game
initializeGame { game, rules } deck =
    let
        initialDraw =
            draw rules.initialDraw
    in
        { game | deck = deck }
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


draw : Int -> Game -> Game
draw count game =
    { game
        | deck = List.drop count game.deck
        , hand = game.hand ++ (List.take count game.deck)
    }


endTurn : Game -> Game
endTurn game =
    let
        { turns } =
            game

        newTurns =
            { turns
                | current =
                    { round = turns.current.round + 1
                    , plays = []
                    }
                , history = turns.history ++ [ turns.current ]
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
            List.partition (\card -> card.id == id) hand

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
