module Update exposing (Msg, Msg(..), buildCombat, findPieceCard, getResourceGain, isCardPlayable, update)

import AllDict
import Array
import Dict
import Random
import Random.Array
import Data.Common exposing (..)
import Model exposing (emptyGame, invalidCard, ordPiece)


-- ACTIONS


type Msg
    = NoOp
    | EndTurn
    | PlayCard Position
    | SelectCard PieceId
    | StartGame
    | InitGame (List GameCard)


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
                    |> gainResources model

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
                |> AllDict.fromList ordPiece

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


gainResources : Model -> Game -> Game
gainResources model game =
    let
        getAmount ( resource, value ) =
            ( resource, value + getResourceGain model resource )
    in
        { game | resources = List.map getAmount model.game.resources }


getResourceGain : Model -> Resource -> ResourceCount
getResourceGain model resource =
    getInitialGain model.rules resource + getEffectGain model resource


getEffectGain : Model -> Resource -> ResourceCount
getEffectGain model resource =
    let
        getCardEffects =
            .card >> findPieceCard model >> .effects

        effects =
            List.map getCardEffects model.game.board
                |> List.concat

        getGain effect =
            case effect of
                Gain ( effectResource, count ) ->
                    if effectResource == resource then
                        count
                    else
                        0

                Summon _ ->
                    0
    in
        List.map getGain effects
            |> List.sum


getInitialGain : Rules -> Resource -> ResourceCount
getInitialGain rules resource =
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

        create id =
            GameCard (PieceId id)
    in
        Random.list cardCount pick
            |> Random.map (List.indexedMap create)


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
playCard model newPlay =
    let
        { game } =
            model

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
            , resources = payCard model newPlay
            , turns = newTurns
        }


payCard : Model -> Play -> Resources
payCard model play =
    let
        card =
            findPieceCard model play.card
    in
        List.foldl payCost model.game.resources card.cost


payCost : ResourceAmount -> Resources -> Resources
payCost ( costResource, costCount ) resources =
    let
        paySingle ( resource, count ) =
            if resource == costResource then
                ( resource, count - costCount )
            else
                ( resource, count )
    in
        List.map paySingle resources


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
    AllDict.get id game.cards
        |> Maybe.map (findCard cards)
        |> Maybe.withDefault invalidCard


buildCombat : Model -> Combat
buildCombat model =
    { own = buildCreatures model
    , enemies = buildEnemies model
    }


buildCreatures : Model -> List Creature
buildCreatures model =
    let
        { game } =
            model

        getCardEffects =
            .card >> findPieceCard model >> .effects

        getCreature effect =
            case effect of
                Summon creature ->
                    Just creature

                _ ->
                    Nothing
    in
        List.map getCardEffects game.board
            |> List.concat
            |> List.filterMap getCreature


buildEnemies : Model -> List Creature
buildEnemies { enemies, game } =
    let
        currentRound =
            game.turns.current.round
    in
        Dict.get currentRound enemies
            |> Maybe.withDefault []


isCardPlayable : Model -> Card -> Bool
isCardPlayable { game } card =
    let
        resourceValid ( _, count ) =
            count >= 0
    in
        List.foldl payCost game.resources card.cost
            |> List.all resourceValid


pickRandomCard : Cards -> Random.Generator Card
pickRandomCard cards =
    cards
        |> Array.fromList
        |> Random.Array.sample
        |> Random.map (Maybe.withDefault invalidCard)
