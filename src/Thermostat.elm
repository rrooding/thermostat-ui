module Thermostat exposing (Model, view, angleToTick, tickToTemperature)

import Html exposing (Html, div)
import Svg exposing (Svg, circle, g, text, text_, path, svg)
import Svg.Attributes
    exposing
        ( id
        , width
        , height
        , cx
        , cy
        , r
        , d
        , x
        , y
        , fill
        , style
        , textAnchor
        , alignmentBaseline
        , fontSize
        , fontWeight
        , fontFamily
        , viewBox
        )
import String
import Transform exposing (Point)


--
-- Types
--


type alias Model =
    { setpoint : Float
    , temperature : Float
    , relativeHumidity : Float
    , heating : Bool
    }



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


temperatureTextShiftDegrees : Float
temperatureTextShiftDegrees =
    9



--
-- Public functions
--


view : Model -> List (Svg.Attribute msg) -> Html msg
view model attributes =
    svg
        (svgAttributes ++ attributes)
        [ circle
            [ cx (toString radius)
            , cy (toString radius)
            , r (toString radius)
            , fill (dialColor model.heating)
            , style "transition: fill 0.5s"
            ]
            []
        , dialTicks model
        , centeredText (displayTemperature model.setpoint)
        , text_
            [ x (toString (temperatureTextPosition model).x)
            , y (toString (temperatureTextPosition model).y)
            , fill "white"
            , textAnchor "middle"
            , alignmentBaseline "central"
            , fontSize "15px"
            , fontWeight "bold"
            , fontFamily "Helvetica, sans-serif"
            ]
            [ text (displayTemperature model.temperature) ]
        ]



--
-- Helpers
--


temperatureTextShift : Model -> Float
temperatureTextShift model =
    if model.temperature > model.setpoint then
        temperatureTextShiftDegrees
    else
        -temperatureTextShiftDegrees


temperatureTextPosition : Model -> Point
temperatureTextPosition model =
    let
        angle =
            tickDegrees
                * (model.temperature - minimumTemperature)
                / temperatureRange
                - offsetDegrees
                + temperatureTextShift model
                |> Transform.degreesToRadians
    in
        { x = radius
        , y = (ticksOuterRadius - (ticksOuterRadius - ticksInnerRadius) / 2)
        }
            |> Transform.translatePoint { x = -radius, y = -radius }
            |> Transform.rotatePoint angle
            |> Transform.translatePoint { x = radius, y = radius }


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


displayTemperature : Float -> String
displayTemperature temperature =
    (toString temperature) ++ "°C"


dialColor : Bool -> String
dialColor mode =
    case mode of
        True ->
            "#E36304"

        False ->
            "#222"


svgAttributes : List (Svg.Attribute msg)
svgAttributes =
    [ width "100%"
    , height "100%"
    , viewBox ("0 0 " ++ (toString diameter) ++ " " ++ (toString diameter))
    , style "-user-select: none; -ms-user-select: none; -moz-user-select: none; -khtml-user-select: none; -webkit-user-select: none; -webkit-touch-callout: none;"
    ]


noTextSelectionStyle : List ( String, String )
noTextSelectionStyle =
    [ ( "-webkit-touch-callout", "none" )
    , ( "-webkit-user-select", "none" )
    , ( "-khtml-user-select", "none" )
    , ( "-moz-user-select", "none" )
    , ( "-ms-user-select", "none" )
    , ( "-user-select", "none" )
    ]



--
-- View elements
--


angleToTick : Float -> Int
angleToTick angle =
    angle
        / (tickDegrees / numberOfTicks)
        |> round


tickToTemperature : Int -> Float
tickToTemperature tickNumber =
    let
        temp =
            (toFloat tickNumber) * (temperatureRange / numberOfTicks) + minimumTemperature
    in
        clamp minimumTemperature maximumTemperature ((toFloat (round (temp * 2))) / 2)


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
            List.minimum [ model.temperature, model.setpoint ]
                |> Maybe.withDefault minimumTemperature

        actualMaxValue =
            List.maximum [ model.temperature, model.setpoint ]
                |> Maybe.withDefault maximumTemperature

        min =
            round ((actualMinValue - minimumTemperature) / temperatureRange * numberOfTicks)

        max =
            round ((actualMaxValue - minimumTemperature) / temperatureRange * numberOfTicks)

        list =
            List.range 0 (round numberOfTicks - 1)
    in
        g [] (List.map (dialTick min (max - 1)) list)


centeredText : String -> Svg msg
centeredText content =
    text_
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
