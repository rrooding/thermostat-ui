module Main exposing (..)

import Html exposing (Html)
import Html.App exposing (program)
import Platform.Cmd exposing ((!))
import TouchEvents as Touch
import String
import Svg exposing (Svg, svg, circle, g, text, text', path)
import Transform exposing (Point)
import Svg.Attributes
    exposing
        ( id
        , width
        , height
        , cx
        , cy
        , r
        , x
        , y
        , fill
        , style
        , viewBox
        , textAnchor
        , fontSize
        , fontWeight
        , fontFamily
        , alignmentBaseline
        , d
        )


type HvacMode
    = Heating
    | Cooling
    | Off


type alias Model =
    { targetTemperature : Float
    , ambientTemperature : Float
    , hvacMode : HvacMode
    }


type Msg
    = OnTouchStart Touch.Touch
    | OnTouchMove Touch.Touch
    | OnTouchEnd Touch.Touch



--
-- Configure the layout/settings here
--


diameter : Float
diameter =
    400


tickDegrees : Float
tickDegrees =
    300


offsetDegrees : Float
offsetDegrees =
    180 - (360 - tickDegrees) / 2


numberOfTicks : Float
numberOfTicks =
    160


radius : Float
radius =
    diameter / 2


ticksOuterRadius : Float
ticksOuterRadius =
    diameter / 30


ticksInnerRadius : Float
ticksInnerRadius =
    diameter / 8


minimumTemperature : Float
minimumTemperature =
    10


maximumTemperature : Float
maximumTemperature =
    30


temperatureRange : Float
temperatureRange =
    maximumTemperature - minimumTemperature


ambientTextShiftDegrees : Float
ambientTextShiftDegrees =
    6



--
-- Polygons
--


regularTick : List Point
regularTick =
    [ (Point (radius - 1) ticksOuterRadius)
    , (Point (radius + 1) ticksOuterRadius)
    , (Point (radius + 1) ticksInnerRadius)
    , (Point (radius - 1) ticksInnerRadius)
    ]


largeTick : List Point
largeTick =
    [ (Point (radius - 1.5) ticksOuterRadius)
    , (Point (radius + 1.5) ticksOuterRadius)
    , (Point (radius + 1.5) (ticksInnerRadius + 20))
    , (Point (radius - 1.5) (ticksInnerRadius + 20))
    ]



--
-- Drawing logic here
--


dialColor : HvacMode -> String
dialColor mode =
    case mode of
        Heating ->
            "#E36304"

        Cooling ->
            "#007AF1"

        Off ->
            "#222"


centeredText : String -> Svg msg
centeredText content =
    text'
        [ x (toString radius)
        , y (toString radius)
        , fill "white"
        , textAnchor "middle"
        , alignmentBaseline "central"
        , fontSize "80px"
        , fontWeight "bold"
        , fontFamily "Helvetica, sans-serif"
        ]
        [ text content ]


pointToPath : Int -> Point -> String
pointToPath index point =
    case index of
        0 ->
            "M" ++ (toString point.x) ++ " " ++ (toString point.y)

        _ ->
            "L" ++ (toString point.x) ++ " " ++ (toString point.y)


pointsToPath : List Point -> String
pointsToPath points =
    points
        |> List.indexedMap pointToPath
        |> String.join " "


rotatePoints : Float -> Float -> List Point -> List Point
rotatePoints origin angle points =
    points
        |> List.map (Transform.translatePoint { x = -origin, y = -origin })
        |> List.map (Transform.rotatePoint angle)
        |> List.map (Transform.translatePoint { x = origin, y = origin })


dialTick : Int -> Int -> Int -> Svg msg
dialTick min max tickNumber =
    let
        angle =
            (toFloat tickNumber)
                * (tickDegrees / numberOfTicks)
                - offsetDegrees
                |> Transform.degreesToRadians

        tick =
            if tickNumber == min || tickNumber == max then
                largeTick
            else
                regularTick

        color =
            if tickNumber >= min && tickNumber <= max then
                "rgba(255, 255, 255, 0.8)"
            else
                "rgba(255, 255, 255, 0.3)"
    in
        path
            [ d
                (tick
                    |> rotatePoints radius angle
                    |> pointsToPath
                )
            , fill color
            ]
            []


dialTicks : Model -> Svg msg
dialTicks model =
    let
        actualMinValue =
            List.minimum [ model.ambientTemperature, model.targetTemperature ]
                |> Maybe.withDefault minimumTemperature

        actualMaxValue =
            List.maximum [ model.ambientTemperature, model.targetTemperature ]
                |> Maybe.withDefault maximumTemperature

        min =
            round ((actualMinValue - minimumTemperature) / temperatureRange * numberOfTicks)

        max =
            round ((actualMaxValue - minimumTemperature) / temperatureRange * numberOfTicks)
    in
        g []
            ([0..(round numberOfTicks - 1)]
                |> List.map (dialTick min max)
            )


displayTemperature : Float -> String
displayTemperature temperature =
    (toString temperature) ++ "°C"


ambientTextShift : Model -> Float
ambientTextShift model =
    if model.ambientTemperature > model.targetTemperature then
        ambientTextShiftDegrees
    else
        -ambientTextShiftDegrees


ambientTextPosition : Model -> Point
ambientTextPosition model =
    let
        angle =
            tickDegrees
                * (model.ambientTemperature - minimumTemperature)
                / temperatureRange
                - offsetDegrees
                + ambientTextShift model
                |> Transform.degreesToRadians
    in
        { x = radius
        , y = (ticksOuterRadius - (ticksOuterRadius - ticksInnerRadius) / 2)
        }
            |> Transform.translatePoint { x = -radius, y = -radius }
            |> Transform.rotatePoint angle
            |> Transform.translatePoint { x = radius, y = radius }



--
-- Elm boilerplate stuff
--


init : ( Model, Cmd Msg )
init =
    (Model 15.5 15 Off) ! []


view : Model -> Html Msg
view model =
    svg
        [ width "100%"
        , height "100%"
        , viewBox ("0 0 " ++ (toString diameter) ++ " " ++ (toString diameter))
        , Touch.onTouchStart OnTouchStart
        , Touch.onTouchMove OnTouchMove
        , Touch.onTouchEnd OnTouchEnd
        ]
        [ circle
            [ cx (toString radius)
            , cy (toString radius)
            , r (toString radius)
            , fill (dialColor model.hvacMode)
            , style "transition: fill 0.5s"
            ]
            []
        , dialTicks model
        , centeredText (displayTemperature model.targetTemperature)
        , text'
            [ x (toString (ambientTextPosition model).x)
            , y (toString (ambientTextPosition model).y)
            , fill "white"
            , textAnchor "middle"
            , alignmentBaseline "central"
            , fontSize "15px"
            , fontWeight "bold"
            , fontFamily "Helvetica, sans-serif"
            ]
            [ text (displayTemperature model.ambientTemperature) ]
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnTouchStart touchEvent ->
            { model | targetTemperature = model.targetTemperature + 0.5 } ! []

        _ ->
            model ! []


main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
