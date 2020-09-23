{-# OPTIONS_GHC -Wall #-}

module Test.TestUtils (
    fail,
    testmodule,
    Assertion,
    Test
) where

import           Prelude    hiding (fail)
import           Test.HUnit (Assertion, Test, (@?=), (~:))

fail :: Assertion
fail = True @?= False

testmodule :: String -> Assertion -> Test
testmodule = (~:) . (++) "Test."
