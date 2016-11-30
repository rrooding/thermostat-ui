module Main exposing (..)

import Html exposing (Html, program)
import Html.Events
import Phoenix.Channel
import Phoenix.Socket
import Platform.Cmd exposing ((!))
import Task
import TouchEvents as Touch exposing (Direction(..))
import Transform exposing (Point)
import Thermostat exposing (HvacMode(..), view)
import Window
import Json.Encode as JE
import Json.Decode as JD exposing (field)


type alias Model =
    { lastTouchPositionY : Maybe Float
    , windowCenter : Maybe Point
    , thermostat : Thermostat.Model
    , phxSocket : Phoenix.Socket.Socket Msg
    }


type Msg
    = OnTouchStart Touch.Touch
    | OnTouchMove Touch.Touch
    | UpdateWindowCenter Point
    | PhoenixMsg (Phoenix.Socket.Msg Msg)
    | ReceiveThermostatUpdate JE.Value
    | JoinChannel
    | NoOp


init : ( Model, Cmd Msg )
init =
    ({ lastTouchPositionY = Nothing
     , windowCenter = Nothing
     , thermostat = (Thermostat.Model 15.5 15 Off)
     , phxSocket = initPhxSocket
     }
    )
        ! [ initialWindowCenter ]


initPhxSocket : Phoenix.Socket.Socket Msg
initPhxSocket =
    Phoenix.Socket.init "ws://localhost:4000/socket/websocket"
        |> Phoenix.Socket.withDebug
        |> Phoenix.Socket.on "new:msg" "thermostat:lobby" ReceiveThermostatUpdate


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.button [ Html.Events.onClick JoinChannel ] [ Html.text "Join lobby" ]
        , Thermostat.view model.thermostat
            [ Touch.onTouchStart OnTouchStart
            , Touch.onTouchMove OnTouchMove
            ]
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


type alias ThermostatMessage =
    { wow : String
    }


thermostatDecoder : JD.Decoder ThermostatMessage
thermostatDecoder =
    JD.map ThermostatMessage
        (field "wow" JD.string)


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
                    Phoenix.Channel.init "thermostat:lobby"

                ( phxSocket, phxCmd ) =
                    Phoenix.Socket.join channel model.phxSocket
            in
                ( { model | phxSocket = phxSocket }
                , Cmd.map PhoenixMsg phxCmd
                )

        ReceiveThermostatUpdate raw ->
            let
                a =
                    Debug.log "aaa" "bb"
            in
                case JD.decodeValue thermostatDecoder raw of
                    Ok thermostatMessage ->
                        model ! []

                    Err error ->
                        model ! []

        NoOp ->
            model ! []


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
