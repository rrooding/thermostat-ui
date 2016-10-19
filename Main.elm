module Main exposing (..)

import Html exposing (Html)
import Html.App exposing (program)
import Platform.Cmd exposing ((!))
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
    , hvacMode : HvacMode
    }


type Msg
    = NoOp



--
-- Configure the layout here
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


dialTick : Int -> Svg msg
dialTick tickNumber =
    let
        angle =
            (toFloat tickNumber)
                * (tickDegrees / numberOfTicks)
                - offsetDegrees
                |> Transform.degreesToRadians
    in
        path
            [ d
                (regularTick
                    |> rotatePoints radius angle
                    |> pointsToPath
                )
            , fill "rgba(255, 255, 255, 0.3)"
            ]
            []


dialTicks : Svg msg
dialTicks =
    g []
        ([0..(round numberOfTicks - 1)]
            |> List.map dialTick
        )


displayTemperature : Float -> String
displayTemperature temperature =
    (toString temperature) ++ "°C"



--
-- Elm boilerplate stuff
--


init : ( Model, Cmd Msg )
init =
    (Model 18.4 Off) ! []


view : Model -> Html Msg
view model =
    svg
        [ width "100%"
        , height "100%"
        , viewBox ("0 0 " ++ (toString diameter) ++ " " ++ (toString diameter))
        ]
        [ circle
            [ cx (toString radius)
            , cy (toString radius)
            , r (toString radius)
            , fill (dialColor model.hvacMode)
            , style "transition: fill 0.5s"
            ]
            []
        , dialTicks
        , centeredText (displayTemperature model.targetTemperature)
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


main : Program Never
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
