module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Dict
import Data.Common exposing (..)
import Model exposing (invalidPieceId)
import Update exposing (Msg, Msg(..), buildCombat, findPieceCard, getResourceGain, isCardPlayable)
import View.Styles as Styles exposing (styles)


-- VIEW STRUCTURE


type alias Board =
    List (List BoardCell)


type alias BoardCell =
    { position : Position
    , card : Maybe CardView
    }


type CardStatus
    = Normal
    | Active
    | Unavailable


type alias CardView =
    { id : PieceId
    , card : Card
    , status : CardStatus
    }


type alias GameView =
    { board : Board
    , combat : Combat
    , deck : CardCount
    , hand : List CardView
    , resources : List ResourceInfo
    , turns : List TurnView
    }


type alias PlayView =
    { id : PieceId
    , card : Card
    , position : Position
    }


type alias TurnView =
    { draws : List CardView
    , plays : List PlayView
    , combat : CombatResult
    , round : RoundNumber
    }


buildBoard : Model -> Board
buildBoard model =
    let
        { game } =
            model

        getRange =
            game.board
                |> List.map .position
                |> getBoardRange

        buildPosition { x, y } =
            ( x, y )

        cards =
            game.board
                |> List.map (\p -> ( buildPosition p.position, buildCard model p.card ))
                |> Dict.fromList

        getCell y x =
            { position = { x = x, y = y }
            , card = Dict.get ( x, y ) cards
            }
    in
        getRange .y
            |> List.reverse
            |> List.map (\y -> List.map (getCell y) (getRange .x))


buildCard : Model -> PieceId -> CardView
buildCard model id =
    let
        activeId =
            Maybe.withDefault invalidPieceId model.game.activeCard

        card =
            findPieceCard model id

        status =
            if id == activeId then
                Active
            else if isCardPlayable model card then
                Normal
            else
                Unavailable
    in
        CardView id card status


buildGame : Model -> GameView
buildGame model =
    let
        { cards, game } =
            model

        allTurns =
            [ game.turns.current ] ++ List.reverse game.turns.history
    in
        { board = buildBoard model
        , deck = List.length game.deck
        , combat = buildCombat model
        , hand = List.map (buildCard model) game.hand
        , resources = buildResources model
        , turns = List.map (buildTurn model) allTurns
        }


buildResources : Model -> List ResourceInfo
buildResources model =
    let
        createInfo ( resource, value ) =
            ResourceInfo resource value <| getResourceGain model resource
    in
        List.map createInfo model.game.resources


buildTurn : Model -> Turn -> TurnView
buildTurn model { draws, plays, combat, round } =
    let
        buildDraw card =
            CardView card (findPieceCard model card) Normal

        buildPlay { card, position } =
            PlayView card (findPieceCard model card) position
    in
        { draws = List.map buildDraw draws
        , plays = List.map buildPlay plays
        , combat = combat
        , round = round
        }


getBoardLimit : List Coordinate -> (List Coordinate -> Maybe Coordinate) -> Int -> Coordinate
getBoardLimit coordinates getLimit shift =
    getLimit coordinates
        |> Maybe.map ((+) shift)
        |> Maybe.withDefault 0


getBoardRange : List Position -> (Position -> Coordinate) -> List Coordinate
getBoardRange positions accessor =
    let
        getLimit =
            List.map accessor positions
                |> getBoardLimit
    in
        List.range (getLimit List.minimum -1) (getLimit List.maximum 1)



-- VIEW


view : Model -> Html Msg
view model =
    let
        gameModel =
            buildGame model
    in
        div []
            [ h1 [] [ text "Cottage" ]
            , button [ onClick StartGame ] [ text "Start game" ]
            , button [ onClick EndTurn ] [ text "End turn" ]
            , div [ styles Styles.content ]
                [ gameView gameModel
                , turnsView gameModel.turns
                , cardsView model.cards
                ]
            ]


turnsView : List TurnView -> Html Msg
turnsView turns =
    let
        rows =
            List.map turnView turns
    in
        div [ styles Styles.turns ]
            [ h2 [] [ text "Turns" ]
            , div [] rows
            ]


turnView : TurnView -> Html Msg
turnView turn =
    let
        draws =
            if turn.draws == [] then
                "No cards"
            else
                turn.draws
                    |> List.map cardText
                    |> String.join ", "

        plays =
            if turn.plays == [] then
                "No cards"
            else
                turn.plays
                    |> List.map playText
                    |> String.join ", "
    in
        div []
            [ h4 [] [ text ("Turn " ++ toString turn.round) ]
            , div [] [ text (draws ++ " drawn.") ]
            , div [] [ text (plays ++ " played.") ]
            , div [] [ text ("Combat result: " ++ toString turn.combat) ]
            ]


cardText : CardView -> String
cardText { card, id } =
    let
        (PieceId pieceId) =
            id
    in
        card.name ++ " #" ++ toString pieceId


playText : PlayView -> String
playText { card, id, position } =
    cardText { card = card, id = id, status = Normal } ++ " " ++ positionText position


positionText : Position -> String
positionText { x, y } =
    "(" ++ toString x ++ ", " ++ toString y ++ ")"


gameView : GameView -> Html Msg
gameView model =
    div [ styles Styles.game ]
        [ h2 [] [ text "Board" ]
        , boardView model.board
        , h2 [] [ text "Combat" ]
        , combatView model.combat
        , h2 [] [ text "Resources" ]
        , resourcesView model.resources
        , h2 [] [ text "Hand" ]
        , handView model.hand
        , h2 [] [ text "Deck" ]
        , deckView model.deck
        ]


boardView : Board -> Html Msg
boardView board =
    let
        drawRow =
            \row -> tr [] (List.map boardCell row)
    in
        table []
            (board
                |> List.map drawRow
            )


boardCell : BoardCell -> Html Msg
boardCell cell =
    let
        content =
            case cell.card of
                Just { card } ->
                    cardBar card

                Nothing ->
                    text ""

        playCard =
            case cell.card of
                Nothing ->
                    PlayCard cell.position

                _ ->
                    NoOp

        { y } =
            cell.position

        groundStyle =
            if y < 0 then
                Styles.underground
            else
                []
    in
        td
            [ styles (Styles.slot ++ groundStyle)
            , onClick playCard
            ]
            [ content ]


combatView : Combat -> Html Msg
combatView combat =
    let
        getForces creatures =
            if List.isEmpty creatures then
                "None!"
            else
                List.map .name creatures
                    |> String.join ", "
    in
        div []
            [ div [] [ text ("Enemy forces: " ++ getForces combat.enemies) ]
            , div [] [ text ("Own forces: " ++ getForces combat.own) ]
            ]


resourcesView : List ResourceInfo -> Html Msg
resourcesView resources =
    let
        single { resource, value, roundGain } =
            let
                content =
                    toString resource ++ ": " ++ toString value ++ " (" ++ toString roundGain ++ ")"
            in
                div [] [ text content ]
    in
        div [] <| List.map single resources


deckView : CardCount -> Html Msg
deckView count =
    div [ styles Styles.deck ]
        [ text ("(" ++ (toString count) ++ ")") ]


cardsView : Cards -> Html Msg
cardsView cards =
    div [ styles Styles.cards ]
        [ h2 [] [ text "Cards" ]
        , cardBox cards
        ]


cardBox : Cards -> Html Msg
cardBox cards =
    div [ styles Styles.cardBox ]
        (List.map cardView cards)


cardView : Card -> Html Msg
cardView card =
    div [ styles Styles.card ]
        [ cardBar card
        , div [] (List.map effectView card.effects)
        ]


handView : List CardView -> Html Msg
handView hand =
    let
        handCards =
            List.map gameCardView hand
    in
        div [ styles Styles.cardBox ]
            handCards


gameCardView : CardView -> Html Msg
gameCardView { id, card, status } =
    let
        ( extraStyle, clickAction ) =
            case status of
                Active ->
                    ( Styles.selected, NoOp )

                Normal ->
                    ( [], SelectCard id )

                Unavailable ->
                    ( Styles.unavailable, NoOp )
    in
        div
            [ styles (Styles.card ++ extraStyle)
            , onClick clickAction
            ]
            [ cardBar card
            , div [] (List.map effectView card.effects)
            ]


cardBar : Card -> Html Msg
cardBar card =
    div [ styles Styles.cardBar ]
        [ div [ styles Styles.cardTitle ] [ text card.name ]
        , div [ styles Styles.cardCost ] [ costView card.cost ]
        ]


effectView : Effect -> Html Msg
effectView effect =
    let
        content =
            case effect of
                Gain ( resource, count ) ->
                    if count > 0 then
                        "Gain " ++ resourceText ( resource, count )
                    else
                        "Upkeep " ++ resourceText ( resource, -count )

                Summon creature ->
                    "Summon " ++ creatureText creature
    in
        div [ styles Styles.effect ] [ text content ]


costView : Resources -> Html Msg
costView cost =
    List.map resourceText cost
        |> String.concat
        |> text


creatureText : Creature -> String
creatureText { attack, defense, life } =
    toString attack ++ " / " ++ toString defense ++ ", " ++ toString life ++ " HP"


resourceText : ResourceAmount -> String
resourceText ( resource, count ) =
    toString count ++ resourceSymbol resource


resourceSymbol : Resource -> String
resourceSymbol resource =
    case resource of
        Build ->
            "B"

        Command ->
            "C"

        Magic ->
            "M"
