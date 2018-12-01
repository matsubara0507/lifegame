module Main exposing (Model, Msg(..), main, update, view)

import Board exposing (..)
import Browser
import Html exposing (Html, button, div, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import SingleSlider


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
    , slider : SingleSlider.Model
    }


defaultModel =
    let
        particle =
            30

        defaultSlider =
            SingleSlider.defaultModel

        slider =
            { defaultSlider
                | min = 5.0
                , max = 50.0
                , step = 1.0
                , value = particle
                , minFormatter = always ""
                , maxFormatter = always ""
                , currentValueFormatter = \n _ -> String.fromFloat n
            }
    in
    { interval = 1000
    , board = Board.init particle
    , slider = slider
    }


type Msg
    = SliderMsg SingleSlider.Msg


update msg model =
    case msg of
        SliderMsg smsg ->
            let
                ( updatedSlider, cmd, _ ) =
                    SingleSlider.update smsg model.slider

                updatedBoard =
                    Board.init (truncate updatedSlider.value)
            in
            ( { model | board = updatedBoard, slider = updatedSlider }
            , Cmd.batch [ Cmd.map SliderMsg cmd ]
            )


view model =
    div []
        [ div
            [ style "text-align" "center"
            ]
            [ Html.map SliderMsg (SingleSlider.view model.slider) ]
        , Board.view model.board
        ]


subscriptions =
    \model ->
        Sub.map SliderMsg <| SingleSlider.subscriptions model.slider
