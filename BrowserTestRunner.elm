module BrowserTestRunner exposing (..)

import ElmTest exposing (runSuiteHtml)
import Tests


main =
    runSuiteHtml Tests.all
