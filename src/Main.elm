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
    }


init : ( Model, Cmd Msg )
init =
    ( { board = newBoard
      }
    , Cmd.none
    )


newBoard : Board
newBoard =
    [ 3, 3, 3, 3, 3, 3, 0, 3, 3, 3, 3, 3, 3, 0 ]



-- UPDATE


type Msg
    = Move Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Move hole ->
            ( { model | board = move hole model.board }, Cmd.none )


get : Int -> Board -> Int
get n list =
    Maybe.withDefault 0 <| List.head (List.drop n list)


set : Int -> Int -> Board -> Board
set n stones board =
    List.take n board ++ (stones :: List.drop (n + 1) board)


incrementFrom : Int -> Int -> Board -> ( Board, Int )
incrementFrom hole stones board =
    let
        board' =
            List.indexedMap
                (\i a ->
                    if i >= hole && i < hole + stones && i /= 13 then
                        a + 1
                    else
                        a
                )
                board

        last =
            stones + hole - 1
    in
        if last > 12 then
            incrementFrom 0 (last - 12) board'
        else
            ( board', last )


opposite : Int -> Int
opposite hole =
    12 - hole


steal : Int -> Board -> Board
steal hole board =
    let
        h1 =
            get hole board

        h2 =
            get (opposite hole) board

        h3 =
            get 6 board
    in
        set 6 (h1 + h2 + h3) <| set (opposite hole) 0 (set hole 0 board)


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


moveOpponent : Board -> Board
moveOpponent board =
    let
        holes =
            List.drop 7 board

        move =
            detect (\a -> a > 0) holes
    in
        case move of
            Nothing ->
                board

            Just n ->
                let
                    hole =
                        n + 7

                    stones =
                        get hole board

                    board' =
                        set hole 0 board

                    ( board'', last ) =
                        incrementFrom (hole + 1) stones board'
                in
                    board''


move : Int -> Board -> Board
move hole board =
    let
        stones =
            get hole board

        board' =
            set hole 0 board

        ( board'', last ) =
            incrementFrom (hole + 1) stones board'

        board''' =
            if last >= 0 && last < 6 && get last board == 0 then
                steal last board''
            else
                board''
    in
        if last /= 6 then
            moveOpponent board'''
        else
            board'''



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
            List.map (\stones -> td [] [ text <| toString stones ]) list

        tdPlayerList list =
            let
                playerAttrs i stones =
                    if stones > 0 then
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
    in
        div [ class "container" ]
            [ h1 [] [ text "Kalahelm" ]
            , table [ class "table table-bordered" ]
                [ tbody []
                    [ tr [] ([ kalahaTd kalaha2 ] ++ (tdList holes2) ++ [ kalahaTd kalaha1 ])
                    , tr [] (tdPlayerList holes1)
                    ]
                ]
            ]
