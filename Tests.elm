module Tests where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import String

import CellTest
import BoardTest
import SequenceFinderTest
import DiagonalSequenceFinderTest
import BuildingDiagonalTest

all : Test
all =
    suite "ConnectFour Tests"
        [
            CellTest.tests,
            BoardTest.tests,
            SequenceFinderTest.tests,
            DiagonalSequenceFinderTest.tests,
            BuildingDiagonalTest.tests
        ]
