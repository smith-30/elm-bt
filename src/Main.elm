module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, h1, img, input, p, text)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Task
import Time



-- MODEL


type alias Player =
    { limitOverSec : Int, turn : String }


type alias TimeCounter =
    { counter : Int
    , isStart : Bool
    }


type alias Model =
    { limit : Int
    , turn : Int
    , tc : TimeCounter
    , p1 : Player
    , p2 : Player
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 30 1 { counter = 0, isStart = False } { limitOverSec = 0, turn = "turn" } { limitOverSec = 0, turn = "" }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | DoTimer
    | ChangePlayer
    | NewLimit String
    | Reset


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                ( c, flg ) =
                    if model.tc.isStart then
                        ( model.tc.counter + 1, model.tc.isStart )

                    else
                        ( model.tc.counter, model.tc.isStart )

                isIncrement =
                    if c > model.limit then
                        True

                    else
                        False

                ( updatedP1, updatedP2 ) =
                    if isIncrement then
                        if model.p1.turn == "turn" then
                            let
                                sec =
                                    model.p1.limitOverSec + 1
                            in
                            ( { limitOverSec = sec, turn = "turn" }
                            , model.p2
                            )

                        else
                            let
                                sec =
                                    model.p2.limitOverSec + 1
                            in
                            ( model.p1
                            , { limitOverSec = sec, turn = "turn" }
                            )

                    else
                        ( model.p1, model.p2 )
            in
            ( { model | tc = { counter = c, isStart = flg }, p1 = updatedP1, p2 = updatedP2 }
            , Cmd.none
            )

        DoTimer ->
            let
                flg =
                    not model.tc.isStart

                c =
                    model.tc.counter
            in
            ( { model | tc = { counter = c, isStart = flg } }, Cmd.none )

        ChangePlayer ->
            let
                flg =
                    model.tc.isStart

                ( updatedP1, updatedP2 ) =
                    if model.p2.turn == "turn" then
                        ( { limitOverSec = model.p1.limitOverSec, turn = "turn" }
                        , { limitOverSec = model.p2.limitOverSec, turn = "" }
                        )

                    else
                        ( { limitOverSec = model.p1.limitOverSec, turn = "" }
                        , { limitOverSec = model.p2.limitOverSec, turn = "turn" }
                        )
            in
            ( { model | tc = { counter = 0, isStart = flg }, p1 = updatedP1, p2 = updatedP2 }
            , Cmd.none
            )

        NewLimit l ->
            let
                v =
                    Maybe.withDefault 1 (String.toInt l)

                val =
                    if v == 0 then
                        1

                    else
                        v
            in
            ( { model | limit = val }, Cmd.none )

        Reset ->
            init ()



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.tc.isStart then
        Time.every 1000 Tick

    else
        Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        c =
            String.fromInt model.tc.counter

        p1Over =
            String.fromInt model.p1.limitOverSec

        p2Over =
            String.fromInt model.p2.limitOverSec

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
        [ div [ class "player-2", class model.p2.turn ]
            [ p
                [ class "p-txt" ]
                [ text "Player 2 over: ", text p2Over ]
            ]
        , div [ class "counter" ]
            [ h1 [] [ text c ]
            ]
        , div
            [ class "change" ]
            [ input [ type_ "button", value "Change", onClick ChangePlayer, class "bt change-bt", disabled (not model.tc.isStart) ] []
            ]
        , div [ class "start" ]
            [ input [ type_ "button", value bt, onClick DoTimer, class btClass ] [] ]
        , div [ class "player-1", class model.p1.turn ]
            [ p
                [ class "p-txt" ]
                [ text "Player 1 over: ", text p1Over ]
            ]
        , div [ class "limit-input" ]
            [ input [ type_ "text", value (String.fromInt model.limit), onInput (\l -> NewLimit l), disabled model.tc.isStart ] []
            ]
        , div [ class "reset" ]
            [ input [ type_ "button", value "Reset", onClick Reset, class "bt reset-bt" ] []
            ]
        ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
