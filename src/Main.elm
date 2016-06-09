module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html
import Random


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
    }


type alias Model =
    { move : Move
    , previousMove : Maybe Move
    }


init : ( Model, Cmd Msg )
init =
    ( { move = initMove, previousMove = Nothing }, Cmd.none )


initMove : Move
initMove =
    { board = initBoard, player = 0 }


initBoard : Board
initBoard =
    [ 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0 ]



-- UPDATE


type Msg
    = NextMove Int
    | MoveOpponent
    | MoveOpponentRandom Int
    | Undo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
    Maybe.withDefault 0 <| List.head (List.drop n list)


set : Int -> Int -> Board -> Board
set n stones board =
    List.take n board ++ (stones :: List.drop (n + 1) board)


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

        board' =
            List.indexedMap
                (\i a ->
                    if i >= hole && i < hole + stones + other && i /= kalaha then
                        a + 1
                    else
                        a
                )
                move.board

        move' =
            { move | board = board' }
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

        board =
            set hole 0 move.board
                |> set (opposite hole) 0
                |> set (kalaha move) (h1 + h2 + h3)
    in
        { move | board = board }


nextPlayer : Int -> Int
nextPlayer i =
    (i + 1) % 2


moveOpponent : Int -> Move -> Move
moveOpponent rnd move =
    let
        indexedHoles =
            List.indexedMap (,) move.board
                |> List.filter (\( i, n ) -> n > 0 && i >= 7 && i < 13)

        drop =
            rnd % (List.length indexedHoles - 1)
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


nextMove : Int -> Move -> Move
nextMove hole move =
    let
        board' =
            set hole 0 move.board

        ( move', last ) =
            incrementFrom (hole + 1) { move | board = board' }

        move'' =
            if (ownHole last move) && get last move.board == 0 then
                steal last move'
            else
                move'
    in
        if last /= (kalaha move) then
            { move'' | player = (nextPlayer move.player) }
        else
            move''



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        kalahaTd stones =
            td [ rowspan 2 ] [ text <| toString stones ]

        tdList list =
            let
                attrs stones =
                    if stones > 0 && model.move.player == 1 then
                        [ class "stones" ]
                    else
                        []
            in
                List.map (\stones -> td (attrs stones) [ text <| toString stones ]) list

        tdPlayerList list =
            let
                playerAttrs i stones =
                    if stones > 0 && model.move.player == 0 then
                        [ class "stones playable", onClick (NextMove i) ]
                    else
                        []
            in
                List.indexedMap (\i stones -> td (playerAttrs i stones) [ text <| toString stones ]) list

        kalaha1 =
            get 6 model.move.board

        kalaha2 =
            get 13 model.move.board

        holes1 =
            List.take 6 model.move.board

        holes2 =
            List.drop 1 (List.reverse (List.drop 7 model.move.board))

        nextButton =
            if model.move.player == 0 then
                text ""
            else
                button [ class "btn btn-primary", onClick MoveOpponent ] [ text "Next" ]

        undoButton =
            if model.previousMove == Nothing then
                text ""
            else
                button [ class "btn btn-default", onClick Undo ] [ text "Undo" ]
    in
        div [ class "container" ]
            [ h1 [] [ text <| "Kalahelm" ]
            , table [ class "table table-bordered" ]
                [ tbody []
                    [ tr [] ([ kalahaTd kalaha2 ] ++ (tdList holes2) ++ [ kalahaTd kalaha1 ])
                    , tr [] (tdPlayerList holes1)
                    ]
                ]
            , div [ class "btn-group" ] [ nextButton, undoButton ]
            ]
