module Board exposing (Board, Cell(..), init, view)

import Array exposing (Array)
import Debug
import Html exposing (Html)
import Html.Attributes exposing (style)
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
            [ style "width" (board.particle * unit |> vmin)
            , style "height" (board.particle * unit |> vmin)
            , style "margin-left" "auto"
            , style "margin-right" "auto"
            ]
    in
    concatMapWith (Html.div attr) viewCell board


viewCell : Cell -> Html msg
viewCell cell =
    Html.img
        [ style "width" (unit |> vmin)
        , style "height" (unit |> vmin)
        , style "margin" "0"
        ]
        []


unit : Int
unit =
    3


vmin : Int -> String
vmin n =
    String.append (String.fromInt n) "vmin"
