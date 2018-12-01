module Board exposing (Board, Cell(..), Msg(..), born, concatIndexedMapWith, init, kill, update, view)

import Array exposing (Array)
import Debug
import Html exposing (Html)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onMouseOver)
import String


type alias Board =
    { particle : Int
    , cells : Array Cell
    }


type Cell
    = Dead
    | Alive


init : Int -> Board
init n =
    { particle = n, cells = Array.repeat (n * n) Dead }


concatIndexedMapWith : (List a -> b) -> (Int -> Cell -> a) -> Board -> b
concatIndexedMapWith f g board =
    board.cells
        |> Array.indexedMap g
        |> Array.toList
        |> f


type Msg
    = Born Int


update : Msg -> Board -> Board
update msg board =
    case msg of
        Born idx ->
            born idx board


born : Int -> Board -> Board
born idx board =
    { board | cells = Array.set idx Alive board.cells }


kill : Int -> Board -> Board
kill idx board =
    { board | cells = Array.set idx Dead board.cells }


view : Board -> Html Msg
view board =
    let
        attr =
            [ style "width" (maxLength |> vmin)
            , style "height" (maxLength |> vmin)
            , style "margin-left" "auto"
            , style "margin-right" "auto"
            ]
    in
    concatIndexedMapWith (Html.div attr) (viewCell board) board


viewCell : Board -> Int -> Cell -> Html Msg
viewCell board idx cell =
    let
        styleAttrs =
            [ style "width" (maxLength / toFloat board.particle |> vmin)
            , style "height" (maxLength / toFloat board.particle |> vmin)
            , style "margin" "0"
            , onMouseOver (Born idx)
            ]

        imageLink =
            case cell of
                Dead ->
                    []

                Alive ->
                    [ src "https://avatars0.githubusercontent.com/u/4686622?s=400&v=4" ]
    in
    Html.img (List.append styleAttrs imageLink) []


maxLength : Float
maxLength =
    90.0


vmin : Float -> String
vmin n =
    String.append (String.fromFloat n) "vmin"
