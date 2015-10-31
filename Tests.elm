module Tests where

import ElmTest.Assertion exposing (..)
import ElmTest.Test exposing (..)

import String

import CellTest


all : Test
all =
    suite "ConnectFour Tests"
        [
            CellTest.tests
        ]
