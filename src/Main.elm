module Main exposing (..)

import Html exposing (Html)
import Html.App exposing (program)
import Platform.Cmd exposing ((!))
import Task
import TouchEvents as Touch exposing (Direction(..))
import Transform exposing (Point)
import Thermostat exposing (HvacMode(..), view)
import Window


type alias Model =
    { lastTouchPositionY : Maybe Float
    , windowCenter : Maybe Point
    , thermostat : Thermostat.Model
    }


type Msg
    = OnTouchStart Touch.Touch
    | OnTouchMove Touch.Touch
    | UpdateWindowCenter Point
    | NoOp


init : ( Model, Cmd Msg )
init =
    ({ lastTouchPositionY = Nothing
     , windowCenter = Nothing
     , thermostat = (Thermostat.Model 15.5 15 Off)
     }
    )
        ! [ initialWindowCenter ]


view : Model -> Html Msg
view model =
    Thermostat.view model.thermostat
        [ Touch.onTouchStart OnTouchStart
        , Touch.onTouchMove OnTouchMove
        ]


initialWindowCenter : Cmd Msg
initialWindowCenter =
    Task.perform (\_ -> NoOp) updateWindowCenter Window.size


updateWindowCenter : Window.Size -> Msg
updateWindowCenter windowSize =
    let
        centerOf =
            (\d -> (toFloat d) / 2)
    in
        UpdateWindowCenter (Point (centerOf windowSize.width) (centerOf windowSize.height))


setTargetTemperature : Float -> Thermostat.Model -> Thermostat.Model
setTargetTemperature temperature model =
    { model | targetTemperature = temperature }


angleOfTouchEvent : Model -> Touch.Touch -> Float
angleOfTouchEvent model touch =
    (Point -touch.clientX -touch.clientY)
        |> Transform.translatePoint (Maybe.withDefault (Point 0 0) model.windowCenter)
        |> (\p -> atan2 p.y p.x)
        |> Transform.radiansToDegrees
        |> round
        |> (\d -> (d + 420) % 360)
        |> toFloat


temperatureForAngle : Float -> Float
temperatureForAngle angle =
    angle
        |> Thermostat.angleToTick
        |> Thermostat.tickToTemperature


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTouchStart touchEvent ->
            let
                newTargetTemperature =
                    touchEvent
                        |> (angleOfTouchEvent model)
                        |> temperatureForAngle
            in
                { model | thermostat = (setTargetTemperature newTargetTemperature model.thermostat), lastTouchPositionY = Just touchEvent.clientY } ! []

        OnTouchMove touchEvent ->
            let
                newTargetTemperature =
                    touchEvent
                        |> (angleOfTouchEvent model)
                        |> temperatureForAngle
            in
                { model | thermostat = (setTargetTemperature newTargetTemperature model.thermostat), lastTouchPositionY = Just touchEvent.clientY } ! []

        UpdateWindowCenter windowCenter ->
            { model | windowCenter = Just windowCenter } ! []

        NoOp ->
            model ! []


main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Window.resizes updateWindowCenter
        }
