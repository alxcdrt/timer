port module Main exposing (main)

import Browser
import Time
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes as A exposing (type_, value, class, disabled)

port playSound : String -> Cmd msg

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

type alias RawTime = 
    {
        minutes : String,
        seconds : String
    }

type alias Model = 
    { currentTime : Int
    , startTime : RawTime
    , state : State
    }

init : () -> (Model, Cmd Msg)
init _ =
    ( Model 0 (RawTime "5" "0") Waiting
    , Cmd.none
    )

-- UPDATE

type Msg
    = Start
    | Pause
    | Tick Time.Posix
    | Update TimeMsg String

type TimeMsg
    = Seconds
    | Minutes


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of 
        Tick _ ->
            { model | currentTime = model.currentTime - 1 } |> checkEnded

        Start ->
            ( { model | currentTime = parseTimeRaw model.startTime, state = Running } |> validateStart
            , Cmd.none
            )

        Pause ->
            ( { model | state = if model.state == Stopped then Running else Stopped }
            , Cmd.none
            )

        Update timeMsg newValue ->
            ( updateTime timeMsg model newValue
            , Cmd.none 
            )


updateTime : TimeMsg -> Model -> String -> Model
updateTime msg model newValue =
    case msg of
        Minutes -> { model | startTime = RawTime newValue model.startTime.seconds }
        Seconds -> { model | startTime = RawTime model.startTime.minutes newValue }



validateStart : Model -> Model
validateStart model =
    case model.currentTime of
        0 -> Model 0 (RawTime "5" "0") Waiting
        _ -> model


parseTimeRaw : RawTime -> Int
parseTimeRaw startTime =  
    ( Maybe.withDefault 0 (String.toInt startTime.minutes) ) * 60 + ( Maybe.withDefault 0 (String.toInt startTime.seconds) ) 


checkEnded : Model -> (Model, Cmd Msg)
checkEnded model = 
    if model.currentTime <= 0 
    then ({ model | state = Ended }, playSound "end")
    else (model, Cmd.none)

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
    div [] 
        [ div [class "clock-container"]
            [ div [class "clock"]
                [ div [ timeClasses model ] [ text (minutes ++ " : " ++ seconds) ]
                ]
            , div [class "clock-settings"]
                [ div [] 
                    [ button [ startButtonClasses, onClick Start ] [ text "Start" ]
                    , button [ pauseButtonClasses, onClick Pause, disabled (cantBePaused model.state) ] [ text (pauseButtonText model.state) ]
                    ]
                , input [ class "clock-settings-input-time", type_ "number", A.min "0", A.max "120", value model.startTime.minutes, onInput (Update Minutes) ] []
                , input [ class "clock-settings-input-time", type_ "number", A.min "0", A.max "60", value model.startTime.seconds, onInput (Update Seconds) ] []
                ]
            ]
        ]

cantBePaused : State -> Bool
cantBePaused state = state /= Running && state /= Stopped


pauseButtonText : State -> String
pauseButtonText state =
    case state of
        Stopped -> "Continue"
        _ -> "Pause"

startButtonClasses : Attribute Msg
startButtonClasses = class "clock-settings-button clock-settings-start-button"

pauseButtonClasses : Attribute Msg
pauseButtonClasses = class "clock-settings-button clock-settings-pause-button"

timeClasses : Model -> Attribute Msg
timeClasses model = 
    class (
            "time" 
            ++ ( if model.currentTime <= 10 then " time-ending" else "" )
            ++ ( if model.state == Stopped then " time-paused" else "" ) )
            