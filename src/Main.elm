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
    { radius : Int
    , numTicks : Int
    , hvacMode : HvacMode
    }


type Msg
    = NoOp


dialColor : HvacMode -> String
dialColor mode =
    case mode of
        Heating ->
            "#E36304"

        Cooling ->
            "#007AF1"

        Off ->
            "#222"


centeredText : Int -> String -> Svg msg
centeredText radius content =
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


ticksOuterRadius : Float -> Float
ticksOuterRadius radius =
    (radius * 2) / 30


ticksInnerRadius : Float -> Float
ticksInnerRadius radius =
    (radius * 2) / 8


tickPoints : Float -> List Point
tickPoints radius =
    [ (Point (radius - 1) (ticksOuterRadius radius))
    , (Point (radius + 1) (ticksOuterRadius radius))
    , (Point (radius + 1) (ticksInnerRadius radius))
    , (Point (radius - 1) (ticksInnerRadius radius))
    ]


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


rotatePoints : Int -> Float -> List Point -> List Point
rotatePoints origin angle points =
    points
        |> List.map (Transform.translatePoint { x = -200, y = -200 })
        |> List.map (Transform.rotatePoint angle)
        |> List.map (Transform.translatePoint { x = 200, y = 200 })


dialTick : Model -> Int -> Svg msg
dialTick model num =
    let
        tickDegrees =
            300

        offsetDegrees =
            180 - (360 - tickDegrees) / 2

        angle =
            (toFloat num)
                * (tickDegrees / 100)
                - offsetDegrees
                |> Transform.degreesToRadians
    in
        path
            [ d
                (toFloat model.radius
                    |> tickPoints
                    |> rotatePoints model.radius angle
                    |> pointsToPath
                )
            , fill "rgba(255, 255, 255, 0.3)"
            ]
            []


dialTicks : Model -> Svg msg
dialTicks model =
    g []
        ([0..(model.numTicks - 1)]
            |> List.map (dialTick model)
        )


init : ( Model, Cmd Msg )
init =
    (Model 200 100 Off) ! []


view : Model -> Html Msg
view model =
    svg
        [ width "100%"
        , height "100%"
        , viewBox ("0 0 " ++ (toString (model.radius * 2)) ++ " " ++ (toString (model.radius * 2)))
        ]
        [ circle
            [ cx (toString model.radius)
            , cy (toString model.radius)
            , r (toString model.radius)
            , fill (dialColor model.hvacMode)
            , style "transition: fill 0.5s"
            ]
            []
        , dialTicks model
        , centeredText model.radius "18°C"
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
