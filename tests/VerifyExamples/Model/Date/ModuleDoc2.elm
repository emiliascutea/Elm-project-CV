module VerifyExamples.Model.Date.ModuleDoc2 exposing (..)

-- This file got generated by [elm-verify-examples](https://github.com/stoeffel/elm-verify-examples).
-- Please don't modify this file by hand!

import Test
import Expect

import Model.Date exposing (..)







spec2 : Test.Test
spec2 =
    Test.test "Module VerifyExamples: \n\n    monthsBetween (onlyYear 2020) (full 2021 Jan)\n    --> Nothing" <|
        \() ->
            Expect.equal
                (
                monthsBetween (onlyYear 2020) (full 2021 Jan)
                )
                (
                Nothing
                )