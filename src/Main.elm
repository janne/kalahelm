module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Html.App as Html
import Random
import Svg
import Svg.Attributes exposing (..)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Board =
    List Int


type alias Move =
    { board : Board
    , player : Int
    , winner : Maybe Winner
    }


type Winner
    = Player
    | Opponent
    | Draw


type alias Model =
    { move : Move
    , previousMove : Maybe Move
    }


init : ( Model, Cmd Msg )
init =
    ( { move = initMove, previousMove = Nothing }, Cmd.none )


initMove : Move
initMove =
    { board = initBoard, player = 0, winner = Nothing }


initBoard : Board
initBoard =
    [ 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0 ]



-- UPDATE


type Msg
    = Init
    | NextMove Int
    | MoveOpponent
    | MoveOpponentRandom Int
    | Undo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Init ->
            init

        NextMove hole ->
            ( { model
                | previousMove = Just model.move
                , move = nextMove hole model.move
              }
            , Cmd.none
            )

        MoveOpponent ->
            ( model, Random.generate MoveOpponentRandom (Random.int 0 100) )

        MoveOpponentRandom rnd ->
            ( { model
                | previousMove = Just model.move
                , move = moveOpponent rnd model.move
              }
            , Cmd.none
            )

        Undo ->
            case model.previousMove of
                Nothing ->
                    ( model, Cmd.none )

                Just previousMove ->
                    ( { model | move = previousMove, previousMove = Nothing }, Cmd.none )


get : Int -> Board -> Int
get n list =
    list |> List.drop n |> List.head |> Maybe.withDefault 0


set : Int -> Int -> Board -> Board
set n stones board =
    List.take n board
        ++ [ stones ]
        ++ List.drop (n + 1) board


kalaha : Move -> Int
kalaha move =
    if move.player == 0 then
        6
    else
        13


otherKalaha : Move -> Int
otherKalaha move =
    if move.player == 0 then
        13
    else
        6


stonesMissing : Move -> Int
stonesMissing move =
    36 - List.sum move.board


incrementFrom : Int -> Move -> ( Move, Int )
incrementFrom hole move =
    let
        stones =
            stonesMissing move

        kalaha =
            otherKalaha move

        other =
            if hole < kalaha && hole + stones >= kalaha then
                1
            else
                0

        incrementRange idx value =
            if idx >= hole && idx < hole + stones + other && idx /= kalaha then
                value + 1
            else
                value

        move' =
            { move | board = List.indexedMap incrementRange move.board }
    in
        if stonesMissing move' > 0 then
            incrementFrom 0 move'
        else
            ( move', stones + hole - 1 )


opposite : Int -> Int
opposite hole =
    12 - hole


steal : Int -> Move -> Move
steal hole move =
    let
        h1 =
            get hole move.board

        h2 =
            get (opposite hole) move.board

        h3 =
            get (kalaha move) move.board
    in
        { move
            | board =
                set hole 0 move.board
                    |> set (opposite hole) 0
                    |> set (kalaha move) (h1 + h2 + h3)
        }


nextPlayer : Int -> Move -> Move
nextPlayer hole move =
    if hole /= kalaha move then
        { move | player = (move.player + 1) % 2 }
    else
        move


moveOpponent : Int -> Move -> Move
moveOpponent rnd move =
    let
        indexedHoles =
            List.indexedMap (,) move.board
                |> List.filter (\( i, n ) -> n > 0 && i >= 7 && i < 13)

        drop =
            rnd % List.length indexedHoles
    in
        case List.drop drop indexedHoles |> List.head of
            Nothing ->
                move

            Just ( i, _ ) ->
                nextMove i move


ownHole : Int -> Move -> Bool
ownHole hole move =
    if move.player == 0 then
        hole >= 0 && hole < 6
    else
        hole >= 7 && hole < 13


winner : Move -> Move
winner move =
    { move
        | winner =
            Just
                (case compare (kalahaPlayer move) (kalahaOpponent move) of
                    LT ->
                        Opponent

                    EQ ->
                        Draw

                    GT ->
                        Player
                )
    }


checkWinner : Move -> Move
checkWinner move =
    if List.sum (List.take 6 move.board) == 0 then
        winner
            { move
                | board =
                    List.repeat 6 0
                        ++ [ kalahaPlayer move ]
                        ++ List.repeat 6 0
                        ++ [ 36 - kalahaPlayer move ]
            }
    else if List.sum (List.drop 7 move.board |> List.take 6) == 0 then
        winner
            { move
                | board =
                    List.repeat 6 0
                        ++ [ 36 - kalahaOpponent move ]
                        ++ List.repeat 6 0
                        ++ [ kalahaOpponent move ]
            }
    else
        move


kalahaPlayer : Move -> Int
kalahaPlayer move =
    get 6 move.board


kalahaOpponent : Move -> Int
kalahaOpponent move =
    get 13 move.board


nextMove : Int -> Move -> Move
nextMove hole move =
    let
        ( move', last ) =
            { move | board = set hole 0 move.board }
                |> incrementFrom (hole + 1)

        move'' =
            if (ownHole last move) && get last move.board == 0 then
                steal last move'
            else
                move'
    in
        checkWinner move'' |> nextPlayer last



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


viewTitle : Model -> Html Msg
viewTitle model =
    let
        title winner =
            case winner of
                Nothing ->
                    "Kalaha"

                Just Player ->
                    "Game over, winner was: Human"

                Just Opponent ->
                    "Game over, winner was: Computer"

                Just Draw ->
                    "Game over, it was a draw"
    in
        h1 [] [ text <| title model.move.winner ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        holes1 =
            List.take 6 model.move.board

        holes2 =
            model.move.board
                |> List.drop 7
                |> List.reverse
                |> List.drop 1

        playerAttrs i stones =
            if stones > 0 && model.move.player == 0 then
                [ class "stones playable", onClick (NextMove i) ]
            else
                []

        opponentAttrs i stones =
            if stones > 0 then
                [ class "stones" ]
            else
                []

        drawStone ( x, y ) hole stone =
            let
                gen =
                    Random.pair (Random.int -25 25) (Random.int -25 25)

                seed =
                    Random.initialSeed (x + y + hole + stone)

                ( ( dx, dy ), _ ) =
                    Random.step gen seed
            in
                Svg.circle
                    [ cx (x + 100 * hole + dx |> toString)
                    , cy (y + dy |> toString)
                    , r "10"
                    , fill "red"
                    ]
                    []
    in
        Svg.svg [ width "100%", stroke "black", fill "white", rx "40", ry "40", viewBox "0 0 800 210" ]
            ([ Svg.rect [ width "100%", height "100%", rx "10", ry "10", fill "black" ] [] ]
                ++ List.map (drawStone ( 50, 105 ) 0) [0..kalahaOpponent model.move - 1]
                ++ List.map (drawStone ( 750, 105 ) 0) [0..kalahaPlayer model.move - 1]
                ++ (List.foldl (++) [] <| List.indexedMap (\i cnt -> List.map (drawStone ( 150, 55 ) i) [0..cnt - 1]) holes2)
                ++ (List.indexedMap (\i cnt -> Svg.rect ([ x (100 * i + 105 |> toString), y "10", width "90", height "90", rx "40", ry "40", fillOpacity "0.5" ] ++ (opponentAttrs i cnt)) []) holes2)
                ++ (List.foldl (++) [] <| List.indexedMap (\i cnt -> List.map (drawStone ( 150, 155 ) i) [0..cnt - 1]) holes1)
                ++ (List.indexedMap (\i cnt -> Svg.rect ([ x (100 * i + 105 |> toString), y "110", width "90", height "90", rx "40", ry "40", fillOpacity "0.5" ] ++ (playerAttrs i cnt)) []) holes1)
                ++ [ Svg.rect [ x "10", y "10", width "80", height "190", rx "40", ry "40", fillOpacity "0.5" ] []
                   , Svg.rect [ x "710", y "10", width "80", height "190", rx "40", ry "40", fillOpacity "0.5" ] []
                   ]
            )


viewButtons : Model -> Html Msg
viewButtons model =
    let
        drawButton : Msg -> String -> Bool -> Maybe (Html Msg)
        drawButton msg title primary =
            let
                cl =
                    if primary then
                        "btn btn-primary"
                    else
                        "btn btn-default"

                view =
                    button [ Attr.class cl, onClick msg ] [ text title ]
            in
                Just view

        nextButton =
            if model.move.winner /= Nothing then
                drawButton Init "Restart" True
            else if model.move.player == 1 then
                drawButton MoveOpponent "Next" True
            else
                Nothing

        undoButton =
            if model.previousMove == Nothing then
                Nothing
            else
                drawButton Undo "Undo" False
    in
        div [ Attr.class "btn-group" ]
            (List.filterMap identity [ nextButton, undoButton ])


view : Model -> Html Msg
view model =
    div [ Attr.class "container" ]
        [ viewTitle model
        , viewBoard model
        , viewButtons model
        ]
