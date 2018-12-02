module Main exposing (Model, Msg(..), main, update, view)

import Board exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import SingleSlider
import Time


main : Program () Model Msg
main =
    Browser.element
        { init = always ( defaultModel, Cmd.none )
        , update = update
        , view = view
        , subscriptions = subscriptions
        }


type alias Model =
    { interval : Int
    , board : Board
    , sizeSlider : SingleSlider.Model
    , tickSlider : SingleSlider.Model
    }


defaultModel =
    let
        particle =
            30

        defaultSlider =
            SingleSlider.defaultModel

        sizeSlider =
            { defaultSlider
                | min = 5.0
                , max = 50.0
                , step = 1.0
                , value = particle
                , minFormatter = always ""
                , maxFormatter = always ""
                , currentValueFormatter =
                    \n _ -> String.concat [ "1列のマス数: ", String.fromFloat n ]
            }

        tickSlider =
            { defaultSlider
                | min = 50.0
                , max = 1000.0
                , step = 10.0
                , value = 100.0
                , minFormatter = always ""
                , maxFormatter = always ""
                , currentValueFormatter =
                    \n _ -> String.concat [ "更新間隔: ", String.fromFloat n, "ms" ]
            }
    in
    { interval = 1000
    , board = Board.init particle
    , sizeSlider = sizeSlider
    , tickSlider = tickSlider
    }


type Msg
    = SizeSliderMsg SingleSlider.Msg
    | TickSliderMsg SingleSlider.Msg
    | BoardMsg Board.Msg
    | NextTick


update msg model =
    case msg of
        SizeSliderMsg subMsg ->
            let
                ( updatedSlider, cmd, _ ) =
                    SingleSlider.update subMsg model.sizeSlider

                updatedBoard =
                    Board.init (truncate updatedSlider.value)
            in
            ( { model | board = updatedBoard, sizeSlider = updatedSlider }
            , Cmd.batch [ Cmd.map SizeSliderMsg cmd ]
            )

        TickSliderMsg subMsg ->
            let
                ( updatedSlider, cmd, _ ) =
                    SingleSlider.update subMsg model.tickSlider
            in
            ( { model | tickSlider = updatedSlider }
            , Cmd.batch [ Cmd.map TickSliderMsg cmd ]
            )

        BoardMsg subMsg ->
            ( { model | board = Board.update subMsg model.board }, Cmd.none )

        NextTick ->
            ( { model | board = Board.next model.board }, Cmd.none )


view model =
    let
        sliderAttrs =
            [ style "margin-left" "10px"
            , style "margin-right" "10px"
            ]
    in
    div []
        [ div
            [ style "text-align" "center"
            , style "display" "flex"
            , style "justify-content" "center"
            ]
            [ div sliderAttrs
                [ Html.map SizeSliderMsg (SingleSlider.view model.sizeSlider) ]
            , div sliderAttrs
                [ Html.map TickSliderMsg (SingleSlider.view model.tickSlider) ]
            ]
        , Html.map BoardMsg (Board.view model.board)
        ]


subscriptions model =
    Sub.batch
        [ Sub.map SizeSliderMsg <| SingleSlider.subscriptions model.sizeSlider
        , if model.board.planting then
            Sub.none

          else
            Time.every model.tickSlider.value (always NextTick)
        ]
