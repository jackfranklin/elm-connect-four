import String
import Graphics.Element exposing (Element)

import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)
import ElmTest.Runner.Element exposing (runDisplay)

import SequenceFinder exposing (buildDiagonalFrom)

import ConnectFour exposing (createBoard)

tests : Test
tests =
    suite "ConnectFour"
      [
      ]


main = runDisplay tests
