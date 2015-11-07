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
    foundCell = findCellOnBoard createBoard (1, 1)
  in
    assertEqual { x = 1, y = 1, colour = NoColour } foundCell

fillCellOnBoardAssertion =
  let
    board = createBoard
    expectedCell = { x = 2, y = 1, colour = Red }
    newBoard = fillCellOnBoard (2, 1) Red board
  in
    assertEqual expectedCell (findCellOnBoard newBoard (2, 1))

boardHasWinnerAssertion =
  let
    board =
      createBoard
       |> fillCellOnBoard (1, 1) Yellow
       |> fillCellOnBoard (2, 2) Yellow
       |> fillCellOnBoard (3, 3) Yellow
       |> fillCellOnBoard (4, 4) Yellow
  in
    (assert (boardHasWinner board))


placeColourAssertion =
  let
    board = createBoard |> fillCellOnBoard (0, 0) Yellow
    newBoard = placeCounter 0 Red board
    expected = { x = 0, y = 1, colour = Red }
  in
    assertEqual expected (findCellOnBoard newBoard (0, 1))

placeColourAssertionBoardFull =
  let
    board =
      createBoard
        |> fillCellOnBoard (0, 0) Yellow
        |> fillCellOnBoard (0, 1) Red
        |> fillCellOnBoard (0, 2) Yellow
        |> fillCellOnBoard (0, 3) Yellow
        |> fillCellOnBoard (0, 4) Yellow
        |> fillCellOnBoard (0, 5) Yellow
  in
    assertEqual (placeCounter 0 Red board) board



tests : Test
tests =
    suite "Board Tests"
      [
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
          ],
        suite "#boardHasWinner"
          [
            test "it returns true if the board has a winner"
              boardHasWinnerAssertion
          ],
        suite "#placeColour"
          [
            test "it places the colour in the right slot" placeColourAssertion,
            test "it does nothing if the column is full"
              placeColourAssertionBoardFull
          ]
      ]
