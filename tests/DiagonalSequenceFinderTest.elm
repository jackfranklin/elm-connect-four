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
    test "sequence in row test one"
    (assertEqual expected (sequenceInDiagonal finalBoard 0 0))

tests : Test
tests =
    suite "DiagonalSequenceFinderTest"
      [
        diagonalUpRightTest
      ]
