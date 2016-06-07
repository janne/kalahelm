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
    }


init : ( Model, Cmd Msg )
init =
    ( { board = newBoard
      , player = 0
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


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move hole ->
            ( move hole model, Cmd.none )

        MoveOpponent ->
            ( moveOpponent model, Cmd.none )


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


incrementFrom : Int -> Int -> Model -> ( Model, Int )
incrementFrom hole stones model =
    let
        board' =
            List.indexedMap
                (\i a ->
                    if i >= hole && i < hole + stones && i /= (otherKalaha model) then
                        a + 1
                    else
                        a
                )
                model.board

        last =
            stones + hole - 1

        model' =
            { model | board = board' }
    in
        if last > 13 then
            incrementFrom 0 (last - 12) model'
        else
            ( model', last )


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


detect : (a -> Bool) -> List a -> Maybe Int
detect f list =
    List.foldl
        (\( i, new ) current ->
            if current == Nothing then
                (if f new then
                    Just i
                 else
                    Nothing
                )
            else
                current
        )
        Nothing
        <| List.indexedMap (,) list


nextPlayer : Int -> Int
nextPlayer i =
    (i + 1) % 2


moveOpponent : Model -> Model
moveOpponent model =
    let
        holes =
            List.drop 7 model.board

        move =
            detect (\a -> a > 0) holes
    in
        case move of
            Nothing ->
                model

            Just n ->
                let
                    hole =
                        n + 7

                    stones =
                        get hole model.board

                    board' =
                        set hole 0 model.board

                    ( model', last ) =
                        incrementFrom (hole + 1) stones { model | board = board' }
                in
                    { model' | player = (nextPlayer model.player) }


move : Int -> Model -> Model
move hole model =
    let
        stones =
            get hole model.board

        board' =
            set hole 0 model.board

        ( model', last ) =
            incrementFrom (hole + 1) stones { model | board = board' }

        model'' =
            if last >= 0 && last < 6 && get last model.board == 0 then
                steal last model'
            else
                model'
    in
        if last /= 6 then
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
                        [ class "player-hole" ]
                    else
                        []
            in
                List.map (\stones -> td (attrs stones) [ text <| toString stones ]) list

        tdPlayerList list =
            let
                playerAttrs i stones =
                    if stones > 0 && model.player == 0 then
                        [ class "player-hole", onClick (Move i) ]
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

        nextPlayer =
            if model.player == 0 then
                text ""
            else
                button [ onClick MoveOpponent ] [ text "Next" ]
    in
        div [ class "container" ]
            [ h1 [] [ text <| "Kalahelm" ]
            , table [ class "table table-bordered" ]
                [ tbody []
                    [ tr [] ([ kalahaTd kalaha2 ] ++ (tdList holes2) ++ [ kalahaTd kalaha1 ])
                    , tr [] (tdPlayerList holes1)
                    ]
                ]
            , nextPlayer
            ]
