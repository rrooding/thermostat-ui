module TransformTests exposing (transformTests)

import ElmTest exposing (Test, suite, test, assert)
import Transform exposing (degreesToRadians, rotatePoint, translatePoint)


transformTests : Test
transformTests =
    suite "Transform"
        [ testDegreesToRadians
        , testRotatePoint
        , testTranslatePoint
        ]


testDegreesToRadians : Test
testDegreesToRadians =
    suite ".degreesToRadians"
        [ test "converts degrees to radians"
            (15
                |> degreesToRadians
                |> ((==) 0.2617993877991494)
                |> assert
            )
        ]


testRotatePoint : Test
testRotatePoint =
    suite ".rotatePoint"
        [ test "rotates a point"
            ({ x = 1, y = 2 }
                |> rotatePoint (degreesToRadians 15)
                |> ((==) { x = 0.44828773608402683, y = 2.190670697680657 })
                |> assert
            )
        ]


testTranslatePoint : Test
testTranslatePoint =
    suite ".translatePoint"
        [ test "moves the x axis"
            ({ x = 1, y = 1 }
                |> translatePoint { x = 2, y = 0 }
                |> ((==) { x = 3, y = 1 })
                |> assert
            )
        , test "moves the y axis"
            ({ x = 1, y = 1 }
                |> translatePoint { x = 0, y = 2 }
                |> ((==) { x = 1, y = 3 })
                |> assert
            )
        ]
