module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (style)
import List.Extra exposing (getAt)
import Process
import Random
import Random.List exposing (shuffle)
import String exposing (fromInt)
import Task



---- MODEL ----


type Suit
    = Clubs
    | Diamonds
    | Hearts
    | Spades


type Card
    = Card Suit Int


type alias Deck =
    List Card


type Pile
    = EmptyPile Suit
    | Pile Suit Int Int


fullDeck : Deck
fullDeck =
    [ Clubs, Diamonds, Hearts, Spades ]
        |> List.map
            (\suit ->
                List.range minCardValue maxCardValue
                    |> List.map (Card suit)
            )
        |> List.concat


seats =
    4


startingCard =
    Card Diamonds 7


pileStartValue =
    7


minCardValue =
    6


maxCardValue =
    8


humanPlayer =
    0


type alias Model =
    { hands : List Deck
    , clubs : Pile
    , diamonds : Pile
    , hearts : Pile
    , spades : Pile
    , currentPlayer : Maybe Int
    , winners : List Int
    }


initialModel : Model
initialModel =
    { hands = []
    , clubs = EmptyPile Clubs
    , diamonds = EmptyPile Diamonds
    , hearts = EmptyPile Hearts
    , spades = EmptyPile Spades
    , currentPlayer = Nothing
    , winners = []
    }


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | Shuffle (Deck -> Msg)
    | DealAll Int Deck
    | Play Int Card
    | Pass Int


computerLoop : Model -> ( Model, Cmd Msg )
computerLoop nextState =
    ( nextState, computerPlay dumbStrategy nextState )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Shuffle next ->
            ( model, Random.generate next (shuffle fullDeck) )

        DealAll numPlayers deck ->
            computerLoop <| dealAll deck numPlayers model

        Play playerNum card ->
            computerLoop <| play card playerNum model

        Pass playerNum ->
            computerLoop <| pass playerNum model


type alias Strategy =
    Model -> Msg


type alias TurnStrategy =
    Int -> Deck -> Model -> Msg


mapToComputerStrategy : Model -> TurnStrategy -> Strategy
mapToComputerStrategy model strategy =
    case model.currentPlayer of
        Nothing ->
            always NoOp

        Just playerNum ->
            if playerNum == humanPlayer then
                always NoOp

            else
                case playerHand playerNum model of
                    Just hand ->
                        strategy playerNum hand

                    Nothing ->
                        always NoOp


dumbStrategy : TurnStrategy
dumbStrategy playerNum hand model =
    case List.Extra.find (\c -> canPlay c model) hand of
        Just firstCardYouSee ->
            Play playerNum firstCardYouSee

        _ ->
            Pass playerNum


smartStrategy : TurnStrategy
smartStrategy playerNum hand model =
    NoOp


computerPlay : TurnStrategy -> Model -> Cmd Msg
computerPlay strategy model =
    if model.currentPlayer == Just humanPlayer then
        Cmd.none

    else
        Process.sleep 500
            |> Task.perform
                (\_ -> mapToComputerStrategy model strategy model)


updatePlayerHand : (Deck -> Deck) -> Int -> Model -> Model
updatePlayerHand handUpdate playerNum model =
    { model
        | hands =
            List.Extra.updateAt
                playerNum
                handUpdate
                model.hands
    }


updatePile : Suit -> (Pile -> Pile) -> Model -> Model
updatePile suit pileUpdate model =
    case suit of
        Clubs ->
            { model | clubs = pileUpdate model.clubs }

        Diamonds ->
            { model | diamonds = pileUpdate model.diamonds }

        Hearts ->
            { model | hearts = pileUpdate model.hearts }

        Spades ->
            { model | spades = pileUpdate model.spades }


playerHand : Int -> Model -> Maybe Deck
playerHand playerNum model =
    getAt playerNum model.hands


playerHandSize : Int -> Model -> Int
playerHandSize playerNum model =
    playerHand playerNum model
        |> Maybe.map List.length
        |> Maybe.withDefault 0


inHand : Card -> Int -> Model -> Bool
inHand card playerNum model =
    playerHand playerNum model
        |> Maybe.map (List.member card)
        |> Maybe.withDefault False


pileForSuit : Suit -> Model -> Pile
pileForSuit suit model =
    case suit of
        Clubs ->
            model.clubs

        Diamonds ->
            model.diamonds

        Hearts ->
            model.hearts

        Spades ->
            model.spades


eligibleValues : Pile -> List Int
eligibleValues pile =
    case pile of
        EmptyPile _ ->
            [ 7 ]

        Pile _ bottom top ->
            [ bottom - 1, top + 1 ]
                |> List.filter
                    (\value ->
                        value >= minCardValue && value <= maxCardValue
                    )


isNextTopValue : Int -> Pile -> Bool
isNextTopValue value pile =
    case pile of
        EmptyPile _ ->
            value == 7

        Pile _ _ top ->
            value == top + 1


isNextBottomValue : Int -> Pile -> Bool
isNextBottomValue value pile =
    case pile of
        EmptyPile _ ->
            value == 7

        Pile _ bottom _ ->
            value == bottom - 1


pileParts : Pile -> ( List Card, Maybe Card, List Card )
pileParts pile =
    case pile of
        EmptyPile _ ->
            ( [], Nothing, [] )

        Pile suit bottom top ->
            ( List.range bottom (pileStartValue - 1)
                |> List.map (Card suit)
                |> List.reverse
            , Just <| Card suit pileStartValue
            , List.range (pileStartValue + 1) top
                |> List.map (Card suit)
            )


pileSuit : Pile -> Suit
pileSuit pile =
    case pile of
        EmptyPile suit ->
            suit

        Pile suit _ _ ->
            suit


cardSuit : Card -> Suit
cardSuit card =
    case card of
        Card suit _ ->
            suit


cardValue : Card -> Int
cardValue card =
    case card of
        Card _ value ->
            value


canPlay : Card -> Model -> Bool
canPlay card model =
    if model.diamonds == EmptyPile Diamonds then
        card == startingCard

    else
        case card of
            Card suit value ->
                List.member value <|
                    (pileForSuit suit model |> eligibleValues)


suitOrder : Suit -> Int
suitOrder suit =
    case suit of
        Diamonds ->
            0

        Clubs ->
            1

        Hearts ->
            2

        Spades ->
            3


compareCard : Card -> Card -> Order
compareCard a b =
    case compare (cardSuit a |> suitOrder) (cardSuit b |> suitOrder) of
        EQ ->
            compare (cardValue a) (cardValue b)

        o ->
            o


removeCard : Card -> Deck -> Deck
removeCard card =
    List.filter (not << (==) card)


addCard : Card -> Deck -> Deck
addCard =
    (::)


addToPile : Card -> Pile -> Pile
addToPile card pile =
    let
        value =
            cardValue card

        suit =
            cardSuit card
    in
    if pileSuit pile == suit then
        case pile of
            EmptyPile _ ->
                Pile suit value value

            Pile _ bottom top ->
                if isNextBottomValue value pile then
                    Pile suit value top

                else if isNextTopValue value pile then
                    Pile suit bottom value

                else
                    pile

    else
        pile


isCurrentPlayer : Int -> Model -> Bool
isCurrentPlayer playerNum model =
    model.currentPlayer == Just playerNum


withPlayerNum : (Int -> Model -> Model) -> Model -> Model
withPlayerNum fn model =
    case model.currentPlayer of
        Nothing ->
            model

        Just num ->
            fn num model


complete : Pile -> Bool
complete pile =
    case pile of
        EmptyPile _ ->
            False

        Pile _ bottom top ->
            bottom == minCardValue && top == maxCardValue


checkEndGame : Model -> Model
checkEndGame model =
    if piles model |> List.all complete then
        { model | currentPlayer = Nothing }

    else
        model


updateWinners : Int -> Model -> Model
updateWinners playerNum model =
    if List.member playerNum model.winners then
        model

    else if playerHandSize playerNum model > 0 then
        model

    else
        { model | winners = model.winners ++ [ playerNum ] }


play : Card -> Int -> Model -> Model
play card playerNum model =
    if not <| model.currentPlayer == Just playerNum then
        model

    else if not <| inHand card playerNum model then
        model

    else if not <| canPlay card model then
        model

    else
        model
            |> withPlayerNum (updatePlayerHand (removeCard card))
            |> withPlayerNum updateWinners
            |> updatePile (cardSuit card) (addToPile card)
            |> checkEndGame
            |> withPlayerNum endTurn


pass : Int -> Model -> Model
pass =
    endTurn


nextPlayer : Int -> Model -> Model
nextPlayer playerNum model =
    { model | currentPlayer = Just (playerNum + 1 |> modBy (List.length model.hands)) }


passIfOut : Model -> Model
passIfOut model =
    case model.currentPlayer of
        Just x ->
            let
                handSize =
                    getAt x model.hands |> Maybe.map List.length
            in
            case handSize of
                Just 0 ->
                    endTurn x model

                _ ->
                    model

        Nothing ->
            model


endTurn : Int -> Model -> Model
endTurn playerNum =
    nextPlayer playerNum
        >> passIfOut


clearPiles : Model -> Model
clearPiles model =
    { model
        | hearts = initialModel.hearts
        , clubs = initialModel.clubs
        , diamonds = initialModel.diamonds
        , spades = initialModel.spades
    }


piles : Model -> List Pile
piles model =
    List.map (\p -> p model) [ .diamonds, .clubs, .hearts, .spades ]


dealAll : Deck -> Int -> Model -> Model
dealAll shuffled numPlayers =
    clearPiles
        >> (\m ->
                { m
                    | hands = List.repeat numPlayers []
                    , winners = []
                }
           )
        >> dealOneToPlayer shuffled 1
        >> setStartingPlayer


setStartingPlayer : Model -> Model
setStartingPlayer model =
    model.hands
        |> List.Extra.findIndex (List.member startingCard)
        |> (\playerNum -> { model | currentPlayer = playerNum })


dealOneToPlayer : Deck -> Int -> Model -> Model
dealOneToPlayer shuffled playerNum model =
    case shuffled of
        topCard :: rest ->
            updatePlayerHand (addCard topCard) playerNum model
                |> dealOneToPlayer
                    rest
                    (modBy (List.length model.hands) (playerNum + 1))

        [] ->
            model



---- VIEW ----


suitToString : Suit -> String
suitToString suit =
    case suit of
        Clubs ->
            "♣️"

        Diamonds ->
            "♦️"

        Hearts ->
            "♥️"

        Spades ->
            "♠️"


cardValueToString : Int -> String
cardValueToString value =
    if value == 13 then
        "K"

    else if value == 12 then
        "Q"

    else if value == 11 then
        "J"

    else if value == 1 then
        "A"

    else
        fromInt value


suitColor : Suit -> Color
suitColor suit =
    case suit of
        Diamonds ->
            rgb 0.85 0 0

        Hearts ->
            rgb 0.85 0 0

        Clubs ->
            rgb 0 0 0

        Spades ->
            rgb 0 0 0


playerAttr : Bool -> List (Attribute msg)
playerAttr current =
    width (px 120)
        :: (if current then
                [ Font.bold ]

            else
                []
           )


passButton : Model -> Int -> Element Msg
passButton model playerNum =
    let
        hand =
            playerHand playerNum model

        handCanPlay =
            hand
                |> Maybe.map (List.any (\c -> canPlay c model))
                |> Maybe.withDefault False

        onPress =
            if handCanPlay then
                NoOp

            else
                Pass playerNum
    in
    if isCurrentPlayer playerNum model && not handCanPlay then
        Input.button
            [ Font.glow |> cta ]
            { onPress = Just onPress
            , label = text "Pass"
            }

    else
        none


handEl : Model -> Int -> Deck -> Element Msg
handEl model playerNum hand =
    row [ spacing 10, height (px 70) ] <|
        (column (playerAttr (isCurrentPlayer playerNum model)) <|
            [ text ("Player " ++ fromInt (playerNum + 1))
            , passButton model playerNum
            , playerMedal model playerNum
            ]
        )
            :: List.map (cardButton model playerNum) (List.sortWith compareCard hand)


playerMedal : Model -> Int -> Element Msg
playerMedal model playerNum =
    case List.Extra.elemIndex playerNum model.winners of
        Just 0 ->
            text "🥇"

        Just 1 ->
            text "🥈"

        Just 2 ->
            text "🥉"

        Nothing ->
            none

        _ ->
            text "🆗"


cardShape : Int -> List (Attribute Msg)
cardShape scale =
    [ width (px (50 * scale))
    , height (px (70 * scale))
    , Border.rounded (4 * scale)
    ]


cta : (Color -> Float -> a) -> a
cta f =
    f (rgb 0.3 1 0.7) 2.0


buttonHints =
    False


cardButton : Model -> Int -> Card -> Element Msg
cardButton model playerNum card =
    if not <| playerNum == humanPlayer then
        cardBackEl 1

    else if buttonHints then
        if isCurrentPlayer playerNum model && canPlay card model then
            Input.button
                [ Border.glow |> cta ]
                { onPress = Just (Play playerNum card)
                , label =
                    cardEl 1 card
                }

        else
            cardEl 1 card

    else
        Input.button
            []
            { onPress = Just (Play playerNum card)
            , label =
                cardEl 1 card
            }


cardEl : Int -> Card -> Element Msg
cardEl scale card =
    let
        suit =
            cardSuit card

        value =
            cardValue card

        symbol =
            suitToString suit

        valText =
            cardValueToString value

        parts : List ( String, Int )
        parts =
            if value > pileStartValue then
                [ ( valText, 26 ), ( symbol, 26 * scale ) ]

            else if value < pileStartValue then
                [ ( symbol, 26 * scale ), ( valText, 26 ) ]

            else
                [ ( valText, 17 ), ( symbol, 26 * scale ), ( valText, 17 ) ]
    in
    parts
        |> List.map
            (\( partText, fontSize ) ->
                text partText
                    |> el
                        [ Font.size fontSize
                        , Font.center
                        , Font.bold
                        , centerX
                        , centerY
                        , Font.color <| suitColor suit
                        ]
            )
        |> column
            (cardShape scale
                ++ [ Border.shadow { offset = ( 1, 1 ), size = 0, blur = toFloat scale * 3, color = rgb 0.5 0.5 0.5 }
                   , Background.color <| rgb 1 1 1
                   ]
            )


cardBackEl : Int -> Element Msg
cardBackEl scale =
    el
        (cardShape scale
            ++ [ Border.shadow { offset = ( 1, 1 ), size = 0, blur = toFloat scale * 3, color = rgb 0.5 0.5 0.5 }
               , Background.color <| rgb 1 1 1
               , padding <| 5 * scale
               ]
        )
    <|
        el
            [ width fill
            , height fill
            , Background.color <| rgb 0 0.2 0.6
            , Border.rounded 3
            ]
            none


verticalSpread : (Float -> Attribute Msg) -> Deck -> Element Msg
verticalSpread transform =
    List.Extra.indexedFoldr
        (\i card curEl ->
            el
                [ inFront curEl
                , transform (1.0 * toFloat i)
                ]
                (cardEl 2 card)
        )
        (el [] none)


pileEl : Pile -> Element Msg
pileEl pile =
    let
        scale =
            2
    in
    case pile of
        EmptyPile suit ->
            el
                (cardShape scale
                    ++ [ Border.width 1
                       , Border.dashed
                       , Border.color <| rgb 0.5 0.5 0.5
                       ]
                )
                (el
                    [ centerX
                    , centerY
                    , style "opacity" "0.5" |> htmlAttribute
                    ]
                 <|
                    text <|
                        suitToString suit
                )

        _ ->
            let
                ( bottomCards, maybeRootCard, topCards ) =
                    pileParts pile
            in
            case maybeRootCard of
                Just rootCard ->
                    el
                        [ inFront <| el [ moveDown (35 * scale) ] <| verticalSpread ((*) scale >> moveDown) bottomCards
                        , inFront <| el [ moveUp (35 * scale) ] <| verticalSpread ((*) scale >> moveUp) topCards
                        ]
                        (cardEl scale rootCard)

                Nothing ->
                    none


game : Model -> Element Msg
game model =
    case List.length model.hands of
        0 ->
            none

        _ ->
            column [ spacing 10, width fill ]
                (List.indexedMap
                    (handEl model)
                    model.hands
                    ++ [ row [ width fill, spaceEvenly, padding 100 ] <|
                            ([ .diamonds, .clubs, .hearts, .spades ] |> List.map (\r -> r model |> pileEl))
                       ]
                )


view : Model -> Html Msg
view model =
    layout [ width fill ] <|
        column [ spacing 10, width fill ] <|
            [ Input.button []
                { onPress = Just <| Shuffle (DealAll seats)
                , label =
                    text <|
                        if List.any (always True) model.hands then
                            "Restart Game"

                        else
                            "New Game"
                }
            , game model
            ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
