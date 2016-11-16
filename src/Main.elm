module Main exposing (..)

import Html exposing (..)


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


type alias Card =
    { id : CardId
    , name : String
    , cost : Cost
    }


type alias Cards =
    List Card


type alias Model =
    { cards : Cards
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


initialModel : Model
initialModel =
    { cards = someCards
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Cottage" ]
        , cardList model.cards
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
