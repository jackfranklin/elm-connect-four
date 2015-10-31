module BoardTest where

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)

import ConnectFour exposing (..)


boardCellsAllEmpty : Board -> Bool
boardCellsAllEmpty board =
  List.all (\cell -> (not (cellIsFilled cell))) board

tests : Test
tests =
    suite "Board Tests"
      [
        suite "#createBoard"
          [ test "it creates a board that is 6 x 7"
              (assertEqual 42 (List.length createBoard)),
            test "it creates all the cells empty"
              (assert (boardCellsAllEmpty createBoard))
          ]
      ]
