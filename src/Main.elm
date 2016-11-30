module Main exposing (..)

import Html exposing (Html, program)
import Phoenix.Channel
import Phoenix.Socket
import Platform.Cmd exposing ((!))
import Task
import TouchEvents as Touch exposing (Direction(..))
import Transform exposing (Point)
import Thermostat exposing (view)
import Window
import Json.Encode as JE
import Json.Decode as JD exposing (field)


--
-- Types
--


type alias Model =
    { lastTouchPositionY : Maybe Float
    , windowCenter : Maybe Point
    , thermostat : Thermostat.Model
    , phxSocket : Phoenix.Socket.Socket Msg
    }


type alias ThermostatMessage =
    { temperature : Float
    , relative_humidity : Float
    , setpoint : Float
    , heating : Bool
    }


type Msg
    = OnTouchStart Touch.Touch
    | OnTouchMove Touch.Touch
    | OnTouchEnd Touch.Touch
    | UpdateWindowCenter Point
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | ReceiveThermostatUpdate JE.Value
    | JoinChannel
    | NoOp



--
-- Configuration
--


endpoint : String
endpoint =
    -- "localhost:4000"
    "192.168.2.117:4000"


channelServer : String
channelServer =
    "ws://" ++ endpoint ++ "/socket/websocket"


channelTopic : String
channelTopic =
    "thermostat"



--
-- Phoenix Channel stuff
--


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init channelServer
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "new:msg" channelTopic ReceiveThermostatUpdate


thermostatDecoder : JD.Decoder ThermostatMessage
thermostatDecoder =
    JD.map4 ThermostatMessage
        (field "temperature" JD.float)
        (field "relative_humidity" JD.float)
        (field "setpoint" JD.float)
        (field "heating" JD.bool)


joinChannel : Cmd Msg
joinChannel =
    -- Execute bogus task to trigger JoinChannel
    Task.perform (\_ -> JoinChannel) (Task.succeed 42)


init : ( Model, Cmd Msg )
init =
    ({ lastTouchPositionY = Nothing
     , windowCenter = Nothing
     , thermostat = (Thermostat.Model 10 10 0 False)
     , phxSocket = initPhxSocket
     }
    )
        ! [ Cmd.batch [ initialWindowCenter, joinChannel ] ]


view : Model -> Html Msg
view model =
    Thermostat.view model.thermostat
        [ Touch.onTouchStart OnTouchStart
        , Touch.onTouchMove OnTouchMove
        , Touch.onTouchEnd OnTouchEnd
        ]


initialWindowCenter : Cmd Msg
initialWindowCenter =
    Task.perform updateWindowCenter Window.size


updateWindowCenter : Window.Size -> Msg
updateWindowCenter windowSize =
    let
        centerOf =
            (\d -> (toFloat d) / 2)
    in
        UpdateWindowCenter (Point (centerOf windowSize.width) (centerOf windowSize.height))


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
                { model | thermostat = (updateSetpoint newTargetTemperature model.thermostat), lastTouchPositionY = Just touchEvent.clientY } ! []

        OnTouchMove touchEvent ->
            let
                newTargetTemperature =
                    touchEvent
                        |> (angleOfTouchEvent model)
                        |> temperatureForAngle
            in
                { model | thermostat = (updateSetpoint newTargetTemperature model.thermostat), lastTouchPositionY = Just touchEvent.clientY } ! []

        OnTouchEnd touchEvent ->
            model ! []

        UpdateWindowCenter windowCenter ->
            { model | windowCenter = Just windowCenter } ! []

        PhoenixMsg msg ->
            let
                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.update msg model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        JoinChannel ->
            let
                channel =
                    Phoenix.Channel.init channelTopic

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        ReceiveThermostatUpdate raw ->
            case JD.decodeValue thermostatDecoder raw of
                Ok msg ->
                    { model | thermostat = (updateThermostat model.thermostat msg) } ! []

                Err error ->
                    model ! []

        NoOp ->
            model ! []


updateThermostat : Thermostat.Model -> ThermostatMessage -> Thermostat.Model
updateThermostat thermostat msg =
    { thermostat | temperature = msg.temperature, relativeHumidity = msg.relative_humidity, setpoint = msg.setpoint, heating = msg.heating }


updateSetpoint : Float -> Thermostat.Model -> Thermostat.Model
updateSetpoint temperature model =
    { model | setpoint = temperature }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Phoenix.Socket.listen model.phxSocket PhoenixMsg
        , Window.resizes updateWindowCenter
        ]


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
