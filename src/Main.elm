module Main exposing (..)

import Browser
import Time
import Html exposing (..)
import Html.Events exposing (onClick, onInput)
import Html.Attributes exposing (type_, id, value, class)


-- MAIN

main : Program () Model Msg
main = 
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions}

-- MODEL

type alias Model = 
    { currentTime : Int
    , startTime : String
    , tick : Int
    }

init : () -> (Model, Cmd Msg)
init _ =
    (Model 0 "05:00" 0
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
            ( { model | currentTime = model.currentTime - model.tick, tick = if model.currentTime - model.tick <= 0 || model.tick == 0 then 0 else 1 }
            , Cmd.none
            )

        Start ->
            ( { model | currentTime = String.split ":" model.startTime |> sumTime, tick = 1 }
            , Cmd.none
            )

        Pause ->
            ( { model | tick = if model.tick == 0 && model.currentTime > 0 then 1 else 0 }
            , Cmd.none
            )

        Update newTime ->
            ( { model |  startTime = newTime }
            , Cmd.none 
            )


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


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick

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
            , button [ onClick Pause ] [ text "Pause" ]
        ]
