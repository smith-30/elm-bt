module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html exposing (Html, div, h1, img, input, p, text)
import Html.Attributes exposing (class, disabled, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Task
import Time



-- MODEL


type alias Player =
    { limitOverCount : Int, turn : String }


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
    ( Model 30 1 { counter = 0, isStart = False } { limitOverCount = 0, turn = "turn" } { limitOverCount = 0, turn = "" }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | DoTimer
    | ChangePlayer
    | NewLimit String


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

                ( updatedP1, updatedP2 ) =
                    if model.p2.turn == "turn" then
                        ( { limitOverCount = model.p1.limitOverCount, turn = "turn" }
                        , { limitOverCount = model.p2.limitOverCount, turn = "" }
                        )

                    else
                        ( { limitOverCount = model.p1.limitOverCount, turn = "" }
                        , { limitOverCount = model.p2.limitOverCount, turn = "turn" }
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

        p1Over =
            String.fromInt model.p1.limitOverCount

        p2Over =
            String.fromInt model.p2.limitOverCount

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
        ]


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
