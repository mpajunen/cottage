module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Dict
import Model exposing (..)
import Update exposing (..)


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
    { activeCard : Maybe PieceId
    , board : Board
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
            Maybe.withDefault invalidId model.game.activeCard

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
        { activeCard = game.activeCard
        , board = buildBoard model
        , deck = List.length game.deck
        , hand = List.map (buildCard model) game.hand
        , resources = buildResources model
        , turns = List.map (buildTurn model) allTurns
        }


buildResources : Model -> List ResourceInfo
buildResources { game, rules } =
    let
        createInfo ( resource, value ) =
            ResourceInfo resource value <| getResourceGain rules resource
    in
        List.map createInfo game.resources


buildTurn : Model -> Turn -> TurnView
buildTurn model { draws, plays, round } =
    let
        buildDraw card =
            CardView card (findPieceCard model card) Normal

        buildPlay { card, position } =
            PlayView card (findPieceCard model card) position
    in
        { draws = List.map buildDraw draws
        , plays = List.map buildPlay plays
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
            , div [ style mainStyle ]
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
        div [ style cardsStyle ]
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
            ]


cardText : CardView -> String
cardText { card, id } =
    card.name ++ " #" ++ toString id


playText : PlayView -> String
playText { card, id, position } =
    cardText { card = card, id = id, status = Normal } ++ " " ++ positionText position


positionText : Position -> String
positionText { x, y } =
    "(" ++ toString x ++ ", " ++ toString y ++ ")"


gameView : GameView -> Html Msg
gameView model =
    div [ style gameStyle ]
        [ h2 [] [ text "Board" ]
        , boardView model.board
        , h2 [] [ text "Resources" ]
        , resourcesView model.resources
        , h2 [] [ text "Hand" ]
        , handView model
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
                undergroundStyle
            else
                []
    in
        td
            [ style (slotStyle ++ groundStyle)
            , onClick playCard
            ]
            [ content ]


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
    div [ style deckStyle ]
        [ text ("(" ++ (toString count) ++ ")") ]


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


handView : GameView -> Html Msg
handView model =
    let
        singleView =
            gameCardView model

        handCards =
            List.map singleView model.hand
    in
        div [ style cardBoxStyle ]
            handCards


gameCardView : GameView -> CardView -> Html Msg
gameCardView { activeCard } { id, card, status } =
    let
        ( extraStyle, clickAction ) =
            case status of
                Active ->
                    ( selectedStyle, NoOp )

                Normal ->
                    ( [], SelectCard id )

                Unavailable ->
                    ( unavailableStyle, NoOp )
    in
        div
            [ style (cardStyle ++ extraStyle)
            , onClick clickAction
            ]
            [ cardBar card
            ]


cardBar : Card -> Html Msg
cardBar card =
    div [ style cardBarStyle ]
        [ div [ style cardTitleStyle ] [ text card.name ]
        , div [ style cardCostStyle ] [ costView card.cost ]
        ]


costView : Resources -> Html Msg
costView cost =
    let
        single ( resource, count ) =
            toString count ++ resourceSymbol resource
    in
        List.map single cost
            |> String.concat
            |> text


resourceSymbol : Resource -> String
resourceSymbol resource =
    case resource of
        Build ->
            "B"

        Command ->
            "C"

        Magic ->
            "M"



-- ELEMENT STYLES


type alias Style =
    List ( String, String )


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


selectedStyle : Style
selectedStyle =
    [ ( "background-color", "#aaaaaa" )
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


slotStyle : Style
slotStyle =
    [ ( "border", "1px solid #555555" )
    , ( "height", "60px" )
    , ( "width", "120px" )
    , ( "margin", "5px" )
    ]


undergroundStyle : Style
undergroundStyle =
    [ ( "background-color", "#888888" )
    ]


unavailableStyle : Style
unavailableStyle =
    [ ( "border-color", "#aaaaaa" )
    , ( "color", "#aaaaaa" )
    ]
