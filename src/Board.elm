module Board exposing (Board, Cell(..), init, view)

import Array exposing (Array)
import Debug
import Html exposing (Html)
import Html.Attributes exposing (src, style)
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


concatMapWith : (List a -> b) -> (Cell -> a) -> Board -> b
concatMapWith f g board =
    board.cells
        |> Array.map g
        |> Array.toList
        |> f


view : Board -> Html msg
view board =
    let
        attr =
            [ style "width" (maxLength |> vmin)
            , style "height" (maxLength |> vmin)
            , style "margin-left" "auto"
            , style "margin-right" "auto"
            ]
    in
    concatMapWith (Html.div attr) (viewCell board) board


viewCell : Board -> Cell -> Html msg
viewCell board cell =
    let
        styleAttrs =
            [ style "width" (maxLength / toFloat board.particle |> vmin)
            , style "height" (maxLength / toFloat board.particle |> vmin)
            , style "margin" "0"
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
