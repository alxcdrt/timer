module Main exposing (main)

import Browser
import Time
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, id, value, class)
import Html.Attributes exposing (disabled)


-- MAIN

main : Program () Model Msg
main = 
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions}

-- MODEL

type State
    = Waiting
    | Stopped
    | Running
    | Ended

type alias Model = 
    { currentTime : Int
    , startTime : String
    , state : State
    }

init : () -> (Model, Cmd Msg)
init _ =
    (Model 0 "05:00" Waiting
    , Cmd.none
    )

-- UPDATE

type Msg
    = Start
    | Pause
    | Tick Time.Posix
    | Update String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =

    case msg of 
        Tick _ ->
            ( { model | currentTime = model.currentTime - 1 } |> checkEnded
            , Cmd.none
            )

        Start ->
            ( { model | currentTime = String.split ":" model.startTime |> sumTime, state = Running } |> validateStart
            , Cmd.none
            )

        Pause ->
            ( { model | state = if model.state == Stopped then Running else Stopped }
            , Cmd.none
            )

        Update newTime ->
            ( { model |  startTime = newTime }
            , Cmd.none 
            )

validateStart : Model -> Model
validateStart model =
    case model.currentTime of
        0 -> Model 0 model.startTime Waiting
        _ -> model


sumTime : List String -> Int
sumTime list =
    case list of 
        (h :: m :: _) -> (parseString h * 60) + parseString m
        _ -> 0

parseString : String -> Int
parseString a =
    case String.toInt a of
        Nothing -> 0
        Just n -> n

checkEnded : Model -> Model
checkEnded model = 
    if model.currentTime <= 0 then { model | state = Ended } else model

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
    case model.state of
        Running -> Time.every 1000 Tick
        _ -> Sub.none

-- VIEW

view : Model -> Html Msg
view model =
    let
        minutes = String.fromInt (model.currentTime // 60)
        seconds = String.fromInt (remainderBy 60 model.currentTime)
    in
    div [] [
        div [class "clock"]
            [ div [class "time"] [ text (minutes ++ " : " ++ seconds) ]
            ]
            , input [ id "time-input", type_ "time", value model.startTime, onInput Update ] []
            , button [ onClick Start ] [ text "Start" ]
            , button [ onClick Pause, disabled (cantBePaused model.state) ] [ text (pauseButtonText model.state) ]
        ]

cantBePaused : State -> Bool
cantBePaused state = state /= Running && state /= Stopped


pauseButtonText : State -> String
pauseButtonText state =
    case state of
        Stopped -> "Continue"
        _ -> "Pause"