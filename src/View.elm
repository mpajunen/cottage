module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Dict
import Model exposing (..)
import Update exposing (..)


-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Cottage" ]
        , button [ onClick StartGame ] [ text "Start game" ]
        , button [ onClick EndTurn ] [ text "End turn" ]
        , div [ style mainStyle ]
            [ gameView model
            , turnsView model
            , messageView model.game.messages
            , cardsView model.cards
            ]
        ]


turnsView : Model -> Html Msg
turnsView { game } =
    let
        { turns } =
            game

        allTurns =
            [ turns.current ] ++ (List.reverse turns.history)

        rows =
            allTurns
                |> List.map turnView
    in
        div [ style messagesStyle ]
            [ h2 [] [ text "Turns" ]
            , div [] rows
            ]


turnView : Turn -> Html Msg
turnView turn =
    let
        playText : Play -> String
        playText { card, position } =
            "#" ++ toString card ++ " " ++ showPosition position

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
            , div [] [ text (plays ++ " played.") ]
            ]


showPosition : Position -> String
showPosition ( x, y ) =
    "(" ++ toString x ++ ", " ++ toString y ++ ")"


messageView : Messages -> Html Msg
messageView messages =
    let
        row message =
            li [] [ text message ]

        rows =
            messages
                |> List.filterMap printMessage
                |> List.map row
    in
        div [ style messagesStyle ]
            [ h2 [] [ text "Messages" ]
            , ul [] rows
            ]


printMessage : Msg -> Maybe String
printMessage msg =
    case msg of
        Draw count ->
            Just (toString count ++ " cards drawn.")

        EndTurn ->
            Just "Turn ended."

        InitGame deck ->
            Just "Game started."

        PlayCard position ->
            Just "Card played."

        SelectCard id ->
            Nothing

        _ ->
            Nothing


gameView : Model -> Html Msg
gameView { cards, game } =
    div [ style gameStyle ]
        [ h2 [] [ text "Board" ]
        , boardView cards game.board
        , h2 [] [ text "Hand" ]
        , handView cards game
        , h2 [] [ text "Deck" ]
        , button [ onClick (Draw 1) ] [ text "Draw" ]
        , deckView game.deck
        ]


boardView : Cards -> BoardCards -> Html Msg
boardView cards board =
    let
        drawCell =
            boardCell cards

        drawRow =
            \row -> tr [] (List.map drawCell row)
    in
        table []
            (board
                |> getBoard
                |> List.map drawRow
            )


boardCell : Cards -> BoardCell -> Html Msg
boardCell cards cell =
    let
        content =
            case cell.card of
                Just { card } ->
                    card
                        |> findCard cards
                        |> cardBar

                Nothing ->
                    text ""

        playCard =
            case cell.card of
                Nothing ->
                    PlayCard cell.position

                _ ->
                    NoOp
    in
        td
            [ style slotStyle
            , onClick playCard
            ]
            [ content ]


getBoard : BoardCards -> Board
getBoard cards =
    let
        getRange =
            cards
                |> Dict.keys
                |> getBoardRange

        xRange =
            getRange (\( x, _ ) -> x)

        yRange =
            getRange (\( _, y ) -> y)

        getCell x y =
            { position = ( x, y )
            , card = Dict.get ( x, y ) cards
            }
    in
        yRange
            |> List.map (\y -> List.map (\x -> getCell x y) xRange)


getBoardRange : List Position -> (Position -> Coordinate) -> List Coordinate
getBoardRange positions accessor =
    let
        getLimit =
            List.map accessor positions
                |> getBoardLimit
    in
        List.range (getLimit List.minimum -1) (getLimit List.maximum 1)


getBoardLimit : List Coordinate -> (List Coordinate -> Maybe Coordinate) -> Int -> Coordinate
getBoardLimit coordinates getLimit shift =
    getLimit coordinates
        |> Maybe.map ((+) shift)
        |> Maybe.withDefault 0


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


handView : Cards -> Game -> Html Msg
handView cards game =
    let
        singleView =
            gameCardView game

        handCards =
            game.hand
                |> List.map (\card -> ( card.id, findCard cards card.card ))
                |> List.map singleView
    in
        div [ style cardBoxStyle ]
            handCards


gameCardView : Game -> ( PieceId, Card ) -> Html Msg
gameCardView { activeCard } ( id, card ) =
    let
        extraStyle =
            case activeCard of
                Just activeId ->
                    if activeId == id then
                        selectedStyle
                    else
                        []

                Nothing ->
                    []
    in
        div
            [ style (cardStyle ++ extraStyle)
            , onClick (SelectCard id)
            ]
            [ cardBar card
            ]


cardBar : Card -> Html Msg
cardBar card =
    div [ style cardBarStyle ]
        [ div [ style cardTitleStyle ] [ text card.name ]
        , div [ style cardCostStyle ] [ text <| toString card.cost ]
        ]


findCard : Cards -> CardId -> Card
findCard cards id =
    let
        card =
            cards
                |> List.filter (\card -> card.id == id)
                |> List.head
    in
        Maybe.withDefault invalidCard card





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


messagesStyle : Style
messagesStyle =
    [ ( "flex", "1 auto" )
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
