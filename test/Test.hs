{-# OPTIONS_GHC -Wall #-}
module Test (
    module Test.Protocols.Link.Arp,
    runTest
) where

import Test.Protocols.Link.Arp
import Control.Monad (void)
import Test.HUnit (Test, runTestText, putTextToHandle)
import System.IO (stderr)

runTest :: Test -> IO ()
runTest = void . runTestText (putTextToHandle stderr False) 
