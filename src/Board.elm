module Board exposing (Board, Cell(..), Links, Msg(..), born, concatIndexedMapWith, init, kill, next, update, view)

import Array exposing (Array)
import Browser.Dom as Dom exposing (Element)
import Debug
import Html exposing (Html)
import Html.Attributes exposing (id, src, style)
import Html.Events.Extra.Pointer as Pointer
import Html.Events.Extra.Touch as Touch
import String
import Task


type alias Board =
    { size : Int
    , cells : Array Cell
    , planting : Bool
    , links : Links
    , touchPos : ( Float, Float )
    }


type alias Links =
    { alive : String
    , dead : String
    }


type Cell
    = Dead
    | Alive


init : Int -> Links -> Board
init n links =
    { size = n
    , cells = Array.repeat (n * n) Dead
    , planting = False
    , links = links
    , touchPos = ( 0, 0 )
    }


next : Board -> Board
next board =
    { board | cells = Array.indexedMap (nextCell board) board.cells }


nextCell : Board -> Int -> Cell -> Cell
nextCell board idx cell =
    case ( countAroundAliveCell board idx, cell ) of
        ( 2, Alive ) ->
            Alive

        ( 3, _ ) ->
            Alive

        _ ->
            Dead


countAroundAliveCell : Board -> Int -> Int
countAroundAliveCell board idx =
    aroundCell board idx |> List.filter ((==) Alive) |> List.length


aroundCell : Board -> Int -> List Cell
aroundCell board idx =
    [ if modBy board.size idx == 0 then
        -- Left end
        []

      else
        [ idx - board.size - 1, idx - 1, idx + board.size - 1 ]
    , [ idx - board.size, idx + board.size ]
    , if modBy board.size idx == board.size - 1 then
        -- Right end
        []

      else
        [ idx - board.size + 1, idx + 1, idx + board.size + 1 ]
    ]
        |> List.concat
        |> List.filterMap (\n -> Array.get n board.cells)


concatIndexedMapWith : (List a -> b) -> (Int -> Cell -> a) -> Board -> b
concatIndexedMapWith f g board =
    board.cells
        |> Array.indexedMap g
        |> Array.toList
        |> f


type Msg
    = Born Int
    | Planting
    | TouchMovePos ( Float, Float )
    | BornWithTouch (Maybe Element)


update : Msg -> Board -> ( Board, Cmd Msg )
update msg board =
    case msg of
        Born idx ->
            ( born idx board, Cmd.none )

        Planting ->
            ( { board | planting = xor board.planting True }, Cmd.none )

        TouchMovePos pos ->
            ( { board | touchPos = pos }
            , Dom.getElement "board"
                |> Task.attempt (BornWithTouch << Result.toMaybe)
            )

        BornWithTouch Nothing ->
            ( board, Cmd.none )

        BornWithTouch (Just elem) ->
            let
                ( px, py ) =
                    ( elem.element.width / toFloat board.size
                    , elem.element.height / toFloat board.size
                    )

                ( tx, ty ) =
                    board.touchPos

                ( x, y ) =
                    ( (tx - elem.element.x) / px |> floor
                    , (ty - elem.element.y) / py |> floor
                    )
            in
            ( born (y * board.size + x) board, Cmd.none )


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
            [ id "board"
            , style "width" (maxLength |> vmin)
            , style "height" (maxLength |> vmin)
            , style "margin-left" "auto"
            , style "margin-right" "auto"
            ]

        getTouchPos event =
            List.head event.targetTouches
                |> Maybe.map .clientPos
                |> Maybe.withDefault ( 0, 0 )

        bornAttr =
            [ Touch.onWithOptions
                "touchmove"
                { stopPropagation = False, preventDefault = True }
                (TouchMovePos << getTouchPos)
            ]
    in
    concatIndexedMapWith (Html.div (attr ++ bornAttr)) (viewCell board) board


viewCell : Board -> Int -> Cell -> Html Msg
viewCell board idx cell =
    let
        styleAttrs =
            [ style "width" (maxLength / toFloat board.size |> vmin)
            , style "height" (maxLength / toFloat board.size |> vmin)
            , style "margin" "0"
            , style "box-sizing" "border-box"
            , style "border" "0.2vmin solid gray"
            ]

        bornAttr =
            if board.planting then
                [ Pointer.onDown (always Planting)
                , Pointer.onOver (always (Born idx))
                ]

            else
                [ Pointer.onDown (always Planting) ]

        imageLink =
            case cell of
                Dead ->
                    [ src board.links.dead ]

                Alive ->
                    [ src board.links.alive ]
    in
    Html.img (List.concat [ styleAttrs, bornAttr, imageLink ]) []


maxLength : Float
maxLength =
    90.0


vmin : Float -> String
vmin n =
    String.append (String.fromFloat n) "vmin"
