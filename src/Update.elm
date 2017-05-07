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
    in
        ( { model | game = game }, Cmd.none )


playGame : Model -> Msg -> Game
playGame model msg =
    let
        { game, rules } =
            model
    in
        case msg of
            EndTurn ->
                endTurn game
                    |> draw rules.roundDraw
                    |> gainResources rules

            PlayCard position ->
                tryPlayCard model position

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
            |> initResources rules


initResources : Rules -> Game -> Game
initResources rules game =
    let
        getAmount { resource, value } =
            ( resource, value )
    in
        { game | resources = List.map getAmount rules.resources }


gainResources : Rules -> Game -> Game
gainResources rules game =
    let
        getAmount ( resource, value ) =
            ( resource, value + getResourceGain rules resource )
    in
        { game | resources = List.map getAmount game.resources }


getResourceGain : Rules -> Resource -> ResourceCount
getResourceGain rules resource =
    List.filter (\r -> r.resource == resource) rules.resources
        |> List.map .roundGain
        |> List.head
        |> Maybe.withDefault 0


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
                | current = Turn [] [] (turn.round + 1)
                , history = turns.history ++ [ turn ]
            }
    in
        { game | turns = newTurns }


tryPlayCard : Model -> Position -> Game
tryPlayCard model position =
    case model.game.activeCard of
        Just card ->
            playCard model (Play card position)

        Nothing ->
            model.game


playCard : Model -> Play -> Game
playCard { game } newPlay =
    let
        { board, hand, turns } =
            game

        { current } =
            turns

        newHand =
            List.filter (\card -> card /= newPlay.card) hand

        plays =
            current.plays ++ [ newPlay ]

        newTurns =
            { turns | current = { current | plays = plays } }
    in
        { game
            | activeCard = Nothing
            , board = board ++ [ newPlay ]
            , hand = newHand
            , turns = newTurns
        }


findCard : Cards -> CardId -> Card
findCard cards id =
    let
        card =
            cards
                |> List.filter (\card -> card.id == id)
                |> List.head
    in
        Maybe.withDefault invalidCard card


findPieceCard : Model -> PieceId -> Card
findPieceCard { cards, game } id =
    Dict.get id game.cards
        |> Maybe.map (findCard cards)
        |> Maybe.withDefault invalidCard


pickRandomCard : Cards -> Random.Generator Card
pickRandomCard cards =
    cards
        |> Array.fromList
        |> Random.Array.sample
        |> Random.map (Maybe.withDefault invalidCard)
