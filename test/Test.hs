{-# OPTIONS_GHC -Wall #-}
module Test (
    module Test.Protocols.Link.Arp,
    runTest
) where

import           Control.Monad           (void)
import           System.IO               (stderr)
import           Test.HUnit              (Test, putTextToHandle, runTestText)
import           Test.Protocols.Link.Arp

runTest :: Test -> IO ()
runTest = void . runTestText (putTextToHandle stderr False)
