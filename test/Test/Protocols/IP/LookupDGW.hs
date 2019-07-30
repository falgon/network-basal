{-# OPTIONS_GHC -Wall #-}
module Test.Protocols.IP.LookupDGW (
    test1,
    test2
) where

import Network.Basal.Protocols.IP.Internal (lookupDGW)

import Data.List (find)
import Data.Maybe (maybe, isJust)
import Test.HUnit (Test, (~:), (@?=))
import System.Process (readProcess)
import Network.Info (IPv4 (..))

test1 :: IO Test
test1 = (~:) "Test.Protocols.IP.LookupDGW.test1: " . (@?=) True . isJust <$> lookupDGW

test2 :: IO Test
test2 = do
    proc <- find ((=="default") . head) . map words . lines <$> readProcess "ip" ["r"] []
    return $ "Test.Protocols.IP.LookupDGW.test2" ~: case proc of
        Nothing -> True @?= False
        Just x -> lookupDGW >>= maybe (True @?= False) (((@?=x !! 2) . show) . IPv4)
