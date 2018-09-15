{-# OPTIONS_GHC -Wall #-}

module Test.Protocols.IP.ICMP.TestUtils (
    testmodule,
    TT.fail,
    TT.Assertion,
    TT.Test
) where

import qualified Test.Protocols.IP.TestUtils as TT

testmodule :: String -> TT.Assertion -> TT.Test
testmodule = TT.testmodule . (++) "IP."
