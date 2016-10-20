module Main exposing (..)

import Html exposing (Html)
import Html.App exposing (program)
import Platform.Cmd exposing ((!))
import TouchEvents as Touch exposing (Direction(..))
import TouchEvents.Extra exposing (getDirectionY)
import Thermostat exposing (HvacMode(..), view)


type alias Model =
    { lastTouchPositionY : Maybe Float
    , thermostat : Thermostat.Model
    }


type Msg
    = OnTouchStart Touch.Touch
    | OnTouchMove Touch.Touch
    | OnTouchEnd Touch.Touch


init : ( Model, Cmd Msg )
init =
    ({ lastTouchPositionY = Nothing
     , thermostat = (Thermostat.Model 15.5 15 Off)
     }
    )
        ! []



-- (Model 15.5 15 Off Nothing) ! []


view : Model -> Html Msg
view model =
    Thermostat.view model.thermostat
        [ Touch.onTouchStart OnTouchStart
        , Touch.onTouchMove OnTouchMove
        , Touch.onTouchEnd OnTouchEnd
        ]


updateTargetTemperature : Maybe Direction -> Thermostat.Model -> Thermostat.Model
updateTargetTemperature direction model =
    let
        change =
            case direction of
                Just Up ->
                    0.5

                Just Down ->
                    -0.5

                _ ->
                    0
    in
        { model | targetTemperature = model.targetTemperature + change }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTouchStart touchEvent ->
            { model | lastTouchPositionY = Just touchEvent.clientY } ! []

        OnTouchMove touchEvent ->
            let
                direction =
                    model.lastTouchPositionY `Maybe.andThen` (\y -> Just <| getDirectionY y touchEvent.clientY)
            in
                { model | thermostat = (updateTargetTemperature direction model.thermostat), lastTouchPositionY = Just touchEvent.clientY } ! []

        OnTouchEnd touchEvent ->
            model ! []


main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
