module TouchEvents.Extra exposing (getDirectionY)

import TouchEvents exposing (Direction(..))


getDirectionY : Float -> Float -> Direction
getDirectionY start end =
    if start > end then
        Up
    else
        Down
