module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.App as Html


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


type alias Model =
    { board : Board
    , player : Int
    , previousBoard : Maybe Board
    , previousPlayer : Int
    }


init : ( Model, Cmd Msg )
init =
    ( { board = newBoard
      , player = 0
      , previousBoard = Nothing
      , previousPlayer = 0
      }
    , Cmd.none
    )


newBoard : Board
newBoard =
    [ 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0 ]



-- UPDATE


type Msg
    = Move Int
    | MoveOpponent
    | Undo


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move hole ->
            ( move hole model, Cmd.none )

        MoveOpponent ->
            ( moveOpponent model, Cmd.none )

        Undo ->
            case model.previousBoard of
                Nothing ->
                    ( model, Cmd.none )

                Just previousBoard ->
                    ( { model
                        | board = previousBoard
                        , previousBoard = Nothing
                        , player = model.previousPlayer
                      }
                    , Cmd.none
                    )


get : Int -> Board -> Int
get n list =
    Maybe.withDefault 0 <| List.head (List.drop n list)


set : Int -> Int -> Board -> Board
set n stones board =
    List.take n board ++ (stones :: List.drop (n + 1) board)


kalaha : Model -> Int
kalaha model =
    if model.player == 0 then
        6
    else
        13


otherKalaha : Model -> Int
otherKalaha model =
    if model.player == 0 then
        13
    else
        6


stonesMissing : Model -> Int
stonesMissing model =
    36 - List.sum model.board


incrementFrom : Int -> Model -> ( Model, Int )
incrementFrom hole model =
    let
        stones =
            stonesMissing model

        kalaha =
            otherKalaha model

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
                model.board

        model' =
            { model | board = board' }
    in
        if stonesMissing model' > 0 then
            incrementFrom 0 model'
        else
            ( model', stones + hole - 1 )


opposite : Int -> Int
opposite hole =
    12 - hole


steal : Int -> Model -> Model
steal hole model =
    let
        h1 =
            get hole model.board

        h2 =
            get (opposite hole) model.board

        h3 =
            get (kalaha model) model.board

        board =
            set hole 0 model.board
                |> set (opposite hole) 0
                |> set (kalaha model) (h1 + h2 + h3)
    in
        { model | board = board }


nextPlayer : Int -> Int
nextPlayer i =
    (i + 1) % 2


moveOpponent : Model -> Model
moveOpponent model =
    let
        indexedHoles =
            List.indexedMap (,) model.board
                |> List.filter (\( i, n ) -> n > 0 && i >= 7 && i < 13)

    in
        case List.head indexedHoles of
            Nothing ->
                model

            Just (i, _) ->
                move i model


ownHole : Int -> Model -> Bool
ownHole hole model =
    if model.player == 0 then
        hole >= 0 && hole < 6
    else
        hole >= 7 && hole < 13


move : Int -> Model -> Model
move hole model =
    let
        board' =
            set hole 0 model.board

        ( model', last ) =
            incrementFrom (hole + 1)
                { model
                    | previousBoard = Just model.board
                    , previousPlayer = model.player
                    , board = board'
                }

        model'' =
            if (ownHole last model) && get last model.board == 0 then
                steal last model'
            else
                model'
    in
        if last /= (kalaha model) then
            { model'' | player = (nextPlayer model.player) }
        else
            model''



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
                    if stones > 0 && model.player == 1 then
                        [ class "stones" ]
                    else
                        []
            in
                List.map (\stones -> td (attrs stones) [ text <| toString stones ]) list

        tdPlayerList list =
            let
                playerAttrs i stones =
                    if stones > 0 && model.player == 0 then
                        [ class "stones playable", onClick (Move i) ]
                    else
                        []
            in
                List.indexedMap (\i stones -> td (playerAttrs i stones) [ text <| toString stones ]) list

        kalaha1 =
            get 6 model.board

        kalaha2 =
            get 13 model.board

        holes1 =
            List.take 6 model.board

        holes2 =
            List.drop 1 (List.reverse (List.drop 7 model.board))

        nextButton =
            if model.player == 0 then
                text ""
            else
                button [ class "btn btn-primary", onClick MoveOpponent ] [ text "Next" ]

        undoButton =
            if model.previousBoard == Nothing then
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
