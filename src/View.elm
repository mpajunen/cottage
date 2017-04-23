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
        , div [ style mainStyle ]
            [ gameView model
            , cardsView model.cards
            ]
        ]


gameView : Model -> Html Msg
gameView { board, cards, player, ui } =
    div [ style gameStyle ]
        [ h2 [] [ text "Board" ]
        , boardView cards board
        , h2 [] [ text "Hand" ]
        , handView ui (findDeckCards cards player.hand)
        , h2 [] [ text "Deck" ]
        , button [ onClick (Draw 1) ] [ text "Draw" ]
        , deckView player.deck
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


handView : Ui -> GameCards -> Html Msg
handView ui cards =
    let
        singleView =
            gameCardView ui
    in
        div [ style cardBoxStyle ]
            (List.map singleView cards)


gameCardView : Ui -> ( PieceId, Card ) -> Html Msg
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


findDeckCards : Cards -> Deck -> GameCards
findDeckCards cards deck =
    deck
        |> List.map (\card -> ( card.id, findCard cards card.card ))


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
