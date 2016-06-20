module Main exposing (..)

import Html exposing (..)
import Html.Attributes as Attr
import Html.Events exposing (..)
import Html.App as Html
import Random
import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import AnimationFrame
import Ease
import Keyboard
import Char


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


type alias Pos =
    ( Int, Int )


type alias Move =
    { board : Board
    , player : Player
    , winner : Maybe Winner
    }


type Player
    = Human
    | Computer


type Level
    = Easy
    | Medium
    | Hard


type Winner
    = Player Player
    | Draw


type alias Model =
    { move : Move
    , history : List Move
    , steps : List (List Pos)
    , aniMove : Maybe Move
    , autoMove : Bool
    , level : Level
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { move = initMove, history = [], steps = [ [] ], aniMove = Nothing, autoMove = False, level = Medium }


initMove : Move
initMove =
    { board = initBoard, player = Human, winner = Nothing }


initBoard : Board
initBoard =
    [ 4, 4, 4, 4, 4, 4, 0, 4, 4, 4, 4, 4, 4, 0 ]


animating model =
    List.isEmpty model.steps |> not


intersection : Move -> Move -> Maybe Move
intersection a b =
    Just { a | board = List.map2 (\a' b' -> Basics.min a' b') a.board b.board }



-- UPDATE


type Msg
    = NextMove Int
    | MoveComputer
    | MoveComputerRandom Int
    | Press Keyboard.KeyCode
    | ChangeLevel Level
    | Tick
    | Undo
    | Restart


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Restart ->
            ( { initModel
                | history = model.move :: model.history
                , steps = animate model.move initMove
                , aniMove = intersection model.move initMove
                , level = model.level
              }
            , Cmd.none
            )

        NextMove hole ->
            let
                next =
                    nextMove hole model.move
            in
                ( { model
                    | history = model.move :: model.history
                    , move = next
                    , steps = animate model.move next
                    , aniMove = intersection model.move next
                  }
                , Cmd.none
                )

        MoveComputer ->
            ( model, Random.generate MoveComputerRandom (Random.int 0 100) )

        MoveComputerRandom rnd ->
            let
                next =
                    moveComputer rnd model.level model.move
            in
                ( { model
                    | history = model.move :: model.history
                    , move = next
                    , steps = animate model.move next
                    , aniMove = intersection model.move next
                    , autoMove = (next.player == Computer && next.winner == Nothing)
                  }
                , Cmd.none
                )

        Undo ->
            case model.history of
                [] ->
                    ( model, Cmd.none )

                head :: tail ->
                    ( { model
                        | move = head
                        , history = tail
                        , steps = animate model.move head
                        , aniMove = intersection model.move head
                      }
                    , Cmd.none
                    )

        Tick ->
            if model.autoMove && model.aniMove == Nothing then
                update MoveComputer model
            else
                ( step model, Cmd.none )

        Press keyCode ->
            if model.aniMove == Nothing then
                if model.move.player == Human && keyCode >= 49 && keyCode < 55 then
                    let
                        hole =
                            keyCode - 49

                        stones =
                            get hole model.move.board
                    in
                        if stones > 0 then
                            update (NextMove <| keyCode - 49) model
                        else
                            ( model, Cmd.none )
                else if (keyCode == 13 || keyCode == 32) then
                    if model.move.winner /= Nothing then
                        update Restart model
                    else if model.move.player == Computer then
                        update MoveComputer model
                    else
                        ( model, Cmd.none )
                else if (Char.fromCode keyCode == 'u') && not (List.isEmpty model.history) then
                    update Undo model
                else
                    ( model, Cmd.none )
            else
                ( model, Cmd.none )

        ChangeLevel lvl ->
            ( { model | level = lvl }, Cmd.none )


animateSteps : Pos -> Pos -> List Pos
animateSteps ( x, y ) ( x', y' ) =
    let
        n =
            60

        steps =
            List.map (\i -> Ease.outQuad (i / n)) [0..n]

        dx =
            x' - x |> toFloat

        dy =
            y' - y |> toFloat
    in
        List.map (\f -> ( round (dx * f) + x, round (dy * f) + y )) steps


animate : Move -> Move -> List (List Pos)
animate from to =
    let
        positions a b =
            let
                diffs =
                    List.map2
                        (\a' b' ->
                            if (snd a') < (snd b') then
                                List.repeat ((snd b') - (snd a')) (fst a')
                            else
                                []
                        )
                        (List.indexedMap (,) b)
                        (List.indexedMap (,) a)
            in
                diffs
                    |> List.map (List.indexedMap (,))
                    |> List.foldl (++) []
                    |> List.map (\( i, n ) -> holeToPos n ((get n b) + i))

        fromHole =
            positions from.board to.board

        toHole =
            positions to.board from.board

        zipped =
            List.map2 (,) fromHole toHole
    in
        List.map (\( a, b ) -> animateSteps a b) zipped


step : Model -> Model
step model =
    let
        tails =
            List.filterMap List.tail model.steps
    in
        { model
            | steps = tails
            , aniMove =
                if tails == [] then
                    Nothing
                else
                    model.aniMove
        }


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
    case move.player of
        Human ->
            6

        Computer ->
            13


otherKalaha : Move -> Int
otherKalaha move =
    case move.player of
        Human ->
            13

        Computer ->
            6


stonesMissing : Move -> Int
stonesMissing move =
    48 - List.sum move.board


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
        { move
            | player =
                case move.player of
                    Human ->
                        Computer

                    Computer ->
                        Human
        }
    else
        move


moveComputer : Int -> Level -> Move -> Move
moveComputer rnd level move =
    let
        possibleMoves : Move -> List Move
        possibleMoves move =
            List.indexedMap (,) move.board
                |> List.filter (\( i, n ) -> n > 0 && i >= 7 && i < 13)
                |> List.map (\( i, _ ) -> nextMove i move)

        onlyTheBest : List ( Move, Int ) -> List Move
        onlyTheBest moves =
            let
                best =
                    moves
                        |> List.sortBy snd
                        |> List.reverse
                        |> List.head
                        |> Maybe.withDefault ( move, 0 )
                        |> snd
            in
                moves
                    |> List.filter (\( _, i ) -> best == i)
                    |> List.map fst

        bestMoves : List Move -> List Move
        bestMoves moves =
            moves
                |> List.map (\m -> ( m, kalahaComputer m ))
                |> onlyTheBest

        bestRecursiveMoves : List Move -> List Move
        bestRecursiveMoves moves =
            moves
                |> List.map
                    (\m ->
                        case m.player of
                            Human ->
                                ( m, kalahaComputer m )

                            Computer ->
                                case possibleMoves m |> bestRecursiveMoves |> bestMoves of
                                    [] ->
                                        ( m, 0 )

                                    m' :: _ ->
                                        ( m, kalahaComputer m' )
                    )
                |> onlyTheBest

        moves =
            case level of
                Easy ->
                    possibleMoves move

                Medium ->
                    possibleMoves move
                        |> bestMoves

                Hard ->
                    possibleMoves move
                        |> bestRecursiveMoves

        pos =
            rnd % (List.length moves)
    in
        moves
            |> List.drop pos
            |> List.head
            |> Maybe.withDefault move


ownHole : Int -> Move -> Bool
ownHole hole move =
    case move.player of
        Human ->
            hole >= 0 && hole < 6

        Computer ->
            hole >= 7 && hole < 13


winner : Move -> Move
winner move =
    { move
        | winner =
            Just
                (case compare (kalahaHuman move) (kalahaComputer move) of
                    LT ->
                        Player Computer

                    EQ ->
                        Draw

                    GT ->
                        Player Human
                )
    }


checkWinner : Move -> Move
checkWinner move =
    if List.sum (List.take 6 move.board) == 0 then
        winner
            { move
                | board =
                    List.repeat 6 0
                        ++ [ kalahaHuman move ]
                        ++ List.repeat 6 0
                        ++ [ 48 - kalahaHuman move ]
            }
    else if List.sum (List.drop 7 move.board |> List.take 6) == 0 then
        winner
            { move
                | board =
                    List.repeat 6 0
                        ++ [ 48 - kalahaComputer move ]
                        ++ List.repeat 6 0
                        ++ [ kalahaComputer move ]
            }
    else
        move


kalahaHuman : Move -> Int
kalahaHuman move =
    get 6 move.board


kalahaComputer : Move -> Int
kalahaComputer move =
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


holeToPos : Int -> Int -> Pos
holeToPos hole stone =
    let
        gen =
            Random.pair (Random.int -25 25) (Random.int -25 25)

        seed =
            Random.initialSeed (x + y + hole + stone)

        ( ( dx, dy ), _ ) =
            Random.step gen seed

        multiplier =
            if hole == 6 || hole == 13 then
                3
            else
                1

        ( x, y ) =
            if hole < 6 then
                ( hole * 100 + 150, 155 )
            else if hole == 6 then
                ( 750, 105 )
            else if hole < 13 then
                ( (12 - hole) * 100 + 150, 55 )
            else
                ( 50, 105 )
    in
        ( x + dx, y + dy * multiplier )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ AnimationFrame.diffs (always Tick), Keyboard.presses Press ]



-- VIEW


viewTitle : Model -> Html Msg
viewTitle model =
    let
        title winner =
            case winner of
                Nothing ->
                    "Kalahelm"

                Just (Player Human) ->
                    "Game over, winner was: Human"

                Just (Player Computer) ->
                    "Game over, winner was: Computer"

                Just Draw ->
                    "Game over, it was a draw"
    in
        h1 [] [ text <| title model.move.winner ]


viewBoard : Model -> Html Msg
viewBoard model =
    let
        move =
            case model.aniMove of
                Nothing ->
                    model.move

                Just m ->
                    m

        holes1 =
            List.take 6 move.board

        holes2 =
            move.board
                |> List.drop 7
                |> List.reverse
                |> List.drop 1

        playerAttrs : Int -> Int -> List (Html.Attribute Msg)
        playerAttrs i stones =
            if stones > 0 && move.player == Human then
                [ class "stones playable", onClick (NextMove i) ]
            else
                []

        opponentAttrs : Int -> List (Html.Attribute Msg)
        opponentAttrs stones =
            if stones > 0 then
                [ class "stones" ]
            else
                []

        animatedStone : Model -> List (Svg Msg)
        animatedStone model =
            let
                positions =
                    List.filterMap List.head model.steps
            in
                List.map (\pos -> circle pos) positions

        circle : Pos -> Svg Msg
        circle ( x, y ) =
            Svg.circle [ cx (toString x), cy (toString y), r "10", fill "red" ] []

        drawStone : Int -> Int -> Svg Msg
        drawStone hole stone =
            holeToPos hole stone |> circle
    in
        Svg.svg [ width "100%", stroke "black", fill "white", rx "40", ry "40", viewBox "0 0 800 210" ]
            ([ Svg.rect [ width "100%", height "100%", rx "10", ry "10", fill "black" ] [] ]
                ++ List.map (drawStone 13) [0..kalahaComputer move - 1]
                ++ List.map (drawStone 6) [0..kalahaHuman move - 1]
                ++ (List.foldl (++) [] <| List.indexedMap (\i cnt -> List.map (drawStone (12 - i)) [0..cnt - 1]) holes2)
                ++ (List.indexedMap (\i cnt -> Svg.rect ([ x (100 * i + 105 |> toString), y "10", width "90", height "90", rx "40", ry "40", fillOpacity "0.5" ] ++ (opponentAttrs cnt)) []) holes2)
                ++ (List.foldl (++) [] <| List.indexedMap (\i cnt -> List.map (drawStone i) [0..cnt - 1]) holes1)
                ++ (List.indexedMap (\i cnt -> Svg.rect ([ x (100 * i + 105 |> toString), y "110", width "90", height "90", rx "40", ry "40", fillOpacity "0.5" ] ++ (playerAttrs i cnt)) []) holes1)
                ++ [ Svg.rect [ x "10", y "10", width "80", height "190", rx "40", ry "40", fillOpacity "0.5" ] []
                   , Svg.text' [ x "50", y "195", fill "black", fontSize "14", textAnchor "middle" ] [ text (kalahaComputer move |> toString) ]
                   , Svg.rect [ x "710", y "10", width "80", height "190", rx "40", ry "40", fillOpacity "0.5" ] []
                   , Svg.text' [ x "750", y "195", fill "black", fontSize "14", textAnchor "middle" ] [ text (kalahaHuman move |> toString) ]
                   ]
                ++ animatedStone model
            )


viewButtons : Model -> Html Msg
viewButtons model =
    let
        nextButton : Bool -> Html Msg
        nextButton disabled =
            if model.move.winner == Nothing then
                button [ class "btn btn-primary pull-left", Attr.disabled (disabled || model.move.player == Human), onClick MoveComputer ] [ text "Next" ]
            else
                button [ class "btn btn-primary pull-left", Attr.disabled disabled, onClick Restart ] [ text "Restart" ]

        levelButtons : Bool -> Html Msg
        levelButtons disabled =
            let
                lvlButton lvl =
                    let
                        cl =
                            if lvl == model.level then
                                "btn btn-default active"
                            else
                                "btn btn-default"
                    in
                        button [ class cl, Attr.disabled disabled, onClick (ChangeLevel lvl) ] [ lvl |> toString |> text ]
            in
                div [ class "btn-group" ] (List.map lvlButton [ Easy, Medium, Hard ])

        undoButton : Bool -> Html Msg
        undoButton disabled =
            button [ class "btn btn-default pull-right", Attr.disabled (disabled || List.isEmpty model.history), onClick Undo ] [ text "Undo" ]

        disabled =
            animating model
    in
        div [ class "center" ]
            [ nextButton disabled
            , levelButtons disabled
            , undoButton disabled
            ]


view : Model -> Html Msg
view model =
    div [ Attr.class "container" ]
        [ viewTitle model
        , viewBoard model
        , viewButtons model
        ]
