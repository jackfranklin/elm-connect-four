import String
import Graphics.Element exposing (Element)

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

import SequenceFinderTest
tests : Test
tests =
    suite "ConnectFour"
      [
        SequenceFinderTest.tests
      ]


main = runDisplay tests
