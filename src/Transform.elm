module Transform exposing (Point, degreesToRadians, rotatePoint, translatePoint)


type alias Point =
    { x : Float
    , y : Float
    }


{-| Convert angular measures from degrees to radians.

    degreesToRadians 15 == 0.2617993878
-}
degreesToRadians : Float -> Float
degreesToRadians degrees =
    degrees * pi / 180


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
