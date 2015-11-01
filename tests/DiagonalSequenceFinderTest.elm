module DiagonalSequenceFinderTest where

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)

import ConnectFour exposing (fillCellOnBoard, createBoard, Colour(..))
import SequenceFinder exposing (sequenceInRow, sequenceInColumn, sequenceInDiagonal)

diagonalUpRightTest =
  let
    firstBoard = fillCellOnBoard createBoard 0 0 Red
    secondBoard = fillCellOnBoard firstBoard 1 1 Red
    finalBoard = fillCellOnBoard secondBoard 2 2 Red
    expected = { sequence = 3, colour = Red }
  in
    test "diagonal up right sequence test"
    (assertEqual expected (sequenceInDiagonal finalBoard 0 0))

diagonalDownRightTest =
  let
    firstBoard = fillCellOnBoard createBoard 3 3 Red
    secondBoard = fillCellOnBoard firstBoard 4 2 Red
    finalBoard = fillCellOnBoard secondBoard 5 1 Red
    expected = { sequence = 3, colour = Red }
  in
    test "diagonal down right sequence test"
    (assertEqual expected (sequenceInDiagonal finalBoard 3 3))

diagonalDownLeftTest =
  let
    firstBoard = fillCellOnBoard createBoard 3 3 Red
    secondBoard = fillCellOnBoard firstBoard 2 2 Red
    finalBoard = fillCellOnBoard secondBoard 1 1 Yellow
    expected = { sequence = 2, colour = Red }
  in
    test "diagonal down left sequence test"
    (assertEqual expected (sequenceInDiagonal finalBoard 3 3))

diagonalUpLeftTest =
  let
    firstBoard = fillCellOnBoard createBoard 3 3 Red
    secondBoard = fillCellOnBoard firstBoard 2 4 Yellow
    finalBoard = fillCellOnBoard secondBoard 1 5 Yellow
    expected = { sequence = 2, colour = Yellow }
  in
    test "diagonal up left sequence test"
    (assertEqual expected (sequenceInDiagonal finalBoard 3 3))

tests : Test
tests =
    suite "DiagonalSequenceFinderTest"
      [
        diagonalUpRightTest, diagonalDownRightTest,
        diagonalDownLeftTest, diagonalUpLeftTest
      ]
