{-# OPTIONS_GHC -Wall #-}

module Test.Protocols.Link.TestUtils (
    testmodule,
    TT.fail,
    TT.Assertion,
    TT.Test
) where

import qualified Test.Protocols.TestUtils as TT

testmodule :: String -> TT.Assertion -> TT.Test
testmodule = TT.testmodule . (++) "Link."
