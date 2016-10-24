module Transform exposing (Point, degreesToRadians, radiansToDegrees, rotatePoint, translatePoint, getDistance, getSlope)


type alias Point =
    { x : Float
    , y : Float
    }


{-| Convert angular measures from degrees to radians.

    degreesToRadians 15 == 0.2617993877991494
-}
degreesToRadians : Float -> Float
degreesToRadians degrees =
    degrees * pi / 180


{-| Convert angular measures from radians to degrees.

    radiansToDegrees 0.2617993877991494 == 15
-}
radiansToDegrees : Float -> Float
radiansToDegrees radians =
    radians * (180 / pi)


{-| Rotate a point by theta radians around the origin.
-}
rotatePoint : Float -> Point -> Point
rotatePoint theta point =
    { x = point.x * (cos theta) - point.y * (sin theta)
    , y = point.x * (sin theta) + point.y * (cos theta)
    }


translatePoint : Point -> Point -> Point
translatePoint origin point =
    { x = point.x + origin.x
    , y = point.y + origin.y
    }


getDistance : Point -> Point -> Float
getDistance origin point =
    let
        xDiff =
            point.x - origin.x

        yDiff =
            point.y - origin.y
    in
        sqrt (xDiff ^ 2 + yDiff ^ 2)


getSlope : Point -> Point -> Float
getSlope origin point =
    (point.y - origin.y) / (point.x - origin.x)
