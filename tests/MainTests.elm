module MainTests exposing (..)

import ElmTest exposing (runSuiteHtml)
import TransformTests exposing (transformTests)


main : Program Never
main =
    runSuiteHtml transformTests
