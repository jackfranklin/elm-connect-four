module DiagonalSequenceFinderTest where

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)

import ConnectFour exposing (
  fillCellOnBoard, createBoard, Colour(..),
  sequenceInRow, sequenceInColumn, longestSequenceOnBoard,
  sequenceInDiagonal)

diagonalUpRightTest =
  let
    board =
      createBoard
        |> fillCellOnBoard (0, 0) Red
        |> fillCellOnBoard (1, 1) Red
        |> fillCellOnBoard (2, 2) Red
    expected = { sequence = 3, colour = Red }
  in
    test "diagonal up right sequence test"
    (assertEqual expected (sequenceInDiagonal board 0 0))

diagonalDownRightTest =
  let
    board =
      createBoard
        |> fillCellOnBoard (3, 3) Red
        |> fillCellOnBoard (4, 2) Red
        |> fillCellOnBoard (5, 1) Red
    expected = { sequence = 3, colour = Red }
  in
    test "diagonal down right sequence test"
    (assertEqual expected (sequenceInDiagonal board 3 3))

diagonalDownLeftTest =
  let
    board =
      createBoard
        |> fillCellOnBoard (3, 3) Red
        |> fillCellOnBoard (2, 2) Red
        |> fillCellOnBoard (1, 1) Yellow
    expected = { sequence = 2, colour = Red }
  in
    test "diagonal down left sequence test"
    (assertEqual expected (sequenceInDiagonal board 3 3))

diagonalUpLeftTest =
  let
    board =
      createBoard
        |> fillCellOnBoard (3, 3) Red
        |> fillCellOnBoard (2, 4) Yellow
        |> fillCellOnBoard (1, 5) Yellow
    expected = { sequence = 2, colour = Yellow }
  in
    test "diagonal up left sequence test"
    (assertEqual expected (sequenceInDiagonal board 3 3))

tests : Test
tests =
    suite "DiagonalSequenceFinderTest"
      [
        diagonalUpRightTest, diagonalDownRightTest,
        diagonalDownLeftTest, diagonalUpLeftTest
      ]
