module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, h1, img, input, text)
import Html.Attributes exposing (class, src, type_, value)
import Html.Events exposing (onClick)
import Task
import Time



-- MODEL


type alias Player =
    { limitOverCount : Int }


type alias TimeCounter =
    { counter : Int
    , isStart : Bool
    }


type alias Model =
    { tc : TimeCounter
    , p1 : Player
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model { counter = 0, isStart = False } { limitOverCount = 0 }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | DoTimer
    | ChangePlayer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                c =
                    if model.tc.isStart then
                        model.tc.counter + 1

                    else
                        model.tc.counter

                flg =
                    model.tc.isStart
            in
            ( { model | tc = { counter = c, isStart = flg } }
            , Cmd.none
            )

        DoTimer ->
            let
                flg =
                    if model.tc.isStart then
                        False

                    else
                        True

                c =
                    model.tc.counter
            in
            ( { model | tc = { counter = c, isStart = flg } }, Cmd.none )

        ChangePlayer ->
            let
                flg =
                    model.tc.isStart
            in
            ( { model | tc = { counter = 0, isStart = flg } }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        c =
            String.fromInt model.tc.counter

        bt =
            if model.tc.isStart then
                "Stop!"

            else
                "Start"

        btClass =
            if model.tc.isStart then
                "bt stop-bt"

            else
                "bt"
    in
    div [ class "grid-container" ]
        [ div [ class "start" ]
            [ div
                [ class "area-overlap start-bt" ]
                [ input [ type_ "button", value bt, onClick DoTimer, class btClass ] []
                ]
            ]
        , div
            [ class "counter" ]
            [ h1 [] [ text c ]
            ]
        , div
            [ class "change" ]
            [ input [ type_ "button", value "Change", onClick ChangePlayer, class "bt change-bt" ] []
            ]
        ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
