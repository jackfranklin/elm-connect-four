module BoardTest where

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)

import ConnectFour exposing (..)


boardCellsAllEmpty : Board -> Bool
boardCellsAllEmpty board =
  List.all (\cell -> (not (cellIsFilled cell))) board

findCellOnBoardAssertion =
  let
    foundCell = findCellOnBoard createBoard 1 1
  in
    assertEqual { x = 1, y = 1, colour = NoColour } foundCell

fillCellOnBoardAssertion =
  let
    board = createBoard
    expectedCell = { x = 2, y = 1, colour = Red }
    newBoard = fillCellOnBoard board 2 1 Red
  in
    assertEqual expectedCell (findCellOnBoard newBoard 2 1)

findRowSequenceAssertion =
  let
    rowIndex = 0
    firstBoard = fillCellOnBoard createBoard 0 rowIndex Red
    secondBoard = fillCellOnBoard firstBoard 1 rowIndex Red
    finalBoard = fillCellOnBoard secondBoard 3 rowIndex Yellow
  in
    assertEqual 2 (biggestSequenceOnRow finalBoard rowIndex)

tests : Test
tests =
    suite "Board Tests"
      [
        suite "#biggestSequenceOnRow"
          [
            test "it finds the largest sequence" findRowSequenceAssertion
          ],
        suite "#createBoard"
          [ test "it creates a board that is 6 x 7"
              (assertEqual 42 (List.length createBoard)),
            test "it creates all the cells empty"
              (assert (boardCellsAllEmpty createBoard))
          ],
        suite "#findCellOnBoard"
          [ test "it returns the cell on the board"
              findCellOnBoardAssertion
          ],
        suite "#fillCellOnBoard"
          [
            test "it fills the cell with given colour"
              fillCellOnBoardAssertion
          ]
      ]
