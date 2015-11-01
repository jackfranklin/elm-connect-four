module SequenceFinderTest where

import String
import ElmTest.Test exposing (test, Test, suite)
import ElmTest.Assertion exposing (assert, assertEqual)

import ConnectFour exposing (fillCellOnBoard, createBoard, Colour(..))
import SequenceFinder exposing (sequenceInRow)

sequenceInRowAssertion =
  let
    rowIndex = 0
    firstBoard = fillCellOnBoard createBoard 0 rowIndex Red
    secondBoard = fillCellOnBoard firstBoard 1 rowIndex Red
    finalBoard = fillCellOnBoard secondBoard 3 rowIndex Yellow
  in
    assertEqual { sequence = 2, colour = Red } (sequenceInRow finalBoard rowIndex)

tests : Test
tests =
    suite "SequenceFinderTest"
      [
        suite "sequenceInRow"
        [
          test "it finds the longest row and returns information about it"
          sequenceInRowAssertion
        ]
      ]
