{-# OPTIONS_GHC -Wall #-}

module Test.Tools.TestUtils (
    testmodule,
    TT.fail,
    TT.Assertion,
    TT.Test
) where

import qualified Test.TestUtils as TT

testmodule :: String -> TT.Assertion -> TT.Test
testmodule = TT.testmodule . (++) "Tools."
