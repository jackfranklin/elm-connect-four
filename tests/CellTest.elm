module CellTest where

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)

import ConnectFour exposing (..)


tests : Test
tests =
    suite "#cellIsFilled"
        [ test "is true when the cell has a colour"
            (assert (cellIsFilled { x = 1, y = 1, colour = Red})),
          test "is false when the cell has no colour"
            (assert (not (cellIsFilled { x = 1, y = 1, colour = NoColour })))
        ]
