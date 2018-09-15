{-# OPTIONS_GHC -Wall #-}
module Test.Protocols.IP.GetDGWMacAddr (
    test1,
    test2
) where

import qualified Test.Protocols.IP.TestUtils as TT

import Network.Basal.Protocols.IP.Internal (lookupDGW, getDGWMacAddr)
import Network.Basal.Protocols.Utils (getDefaultNIface)

import Data.Maybe (isJust)
import Network.Info (name, IPv4 (..))
import System.Process (readProcess)
import System.Directory (findExecutable)
import Test.HUnit ((@?=))

testmodule :: String -> TT.Assertion -> TT.Test
testmodule = TT.testmodule . (++) "GetDGWMacAddr." 

sudoerr :: String 
sudoerr = "test2: The sudo command is required on the system in order to execute with general user authority."

arpscanerr1 :: String
arpscanerr1 = "test2: This unit test needs arp-scan (https://github.com/royhills/arp-scan) but it does not exist on this system. Please setup it on this system." 

arpscanerr2 :: String
arpscanerr2 = "test2: The arp-scan process did not have expected result."

lookupDGWerr :: String
lookupDGWerr = "test2: failed to lookupDGW."

getDGWMacAddrerr :: String
getDGWMacAddrerr = "test2: failed to getDGWMacAddr."

test1 :: IO TT.Test
test1 = testmodule "test1: " . (@?=) True . isJust <$> (flip getDGWMacAddr (5*1000*100) =<< getDefaultNIface)

test2 :: IO TT.Test
test2 = (>>=) (findExecutable "sudo") $ maybe (return $ testmodule sudoerr TT.fail) $ \sudo -> 
    (>>=) (findExecutable "arp-scan") $ maybe (return $ testmodule arpscanerr1 TT.fail) $ \arpscan -> 
        (>>=) lookupDGW $ maybe (return $ testmodule lookupDGWerr TT.fail) $ \ip -> do
            gd <- getDefaultNIface
            (>>=) (getDGWMacAddr gd (5*1000*1000)) $ maybe (return $ testmodule getDGWMacAddrerr TT.fail) $ \maddr -> do
                proc <- lines <$> readProcess sudo [arpscan, "-q", "-I", name gd, show $ IPv4 ip] []
                return $ if length proc > 2 then let ws = words (proc !! 2) in 
                    if length ws > 1 then testmodule "test2: " $ ws !! 1 @?= show maddr else testmodule arpscanerr2 TT.fail
                    else testmodule arpscanerr2 TT.fail
