module SequenceFinderTest where

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)

import ConnectFour exposing (fillCellOnBoard, createBoard, Colour(..))
import SequenceFinder exposing (sequenceInRow)

sequenceInRowTestOne =
  let
    firstBoard = fillCellOnBoard createBoard 0 0 Red
    secondBoard = fillCellOnBoard firstBoard 1 0 Red
    finalBoard = fillCellOnBoard secondBoard 3 0 Yellow
    expected = { sequence = 2, colour = Red }
  in
    test "sequence in row test one"
    (assertEqual expected (sequenceInRow finalBoard 0))

sequenceInRowTestTwo =
  let
    firstBoard = fillCellOnBoard createBoard 0 0 Red
    secondBoard = fillCellOnBoard firstBoard 1 0 Red
    thirdBoard = fillCellOnBoard secondBoard 2 0 Yellow
    fourthBoard = fillCellOnBoard thirdBoard 3 0 Yellow
    finalBoard = fillCellOnBoard fourthBoard 4 0 Yellow
    expected = { sequence = 3, colour = Yellow }
  in
    test "sequence in row test two"
    (assertEqual expected (sequenceInRow finalBoard 0))

sequenceInRowTestThree =
  let
    firstBoard = fillCellOnBoard createBoard 0 0 Red
    secondBoard = fillCellOnBoard firstBoard 1 0 Red
    thirdBoard = fillCellOnBoard secondBoard 2 0 Yellow
    fourthBoard = fillCellOnBoard thirdBoard 3 0 Red
    fifthBoard = fillCellOnBoard fourthBoard 4 0 Red
    finalBoard = fillCellOnBoard fifthBoard 5 0 Red
    expected = { sequence = 3, colour = Red }
  in
    test "sequence in row test three"
    (assertEqual expected (sequenceInRow finalBoard 0))


tests : Test
tests =
    suite "SequenceFinderTest"
      [
        suite "sequenceInRow"
        [
          sequenceInRowTestOne, sequenceInRowTestTwo, sequenceInRowTestThree
        ]
      ]
