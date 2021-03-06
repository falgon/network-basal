{-# OPTIONS_GHC -Wall #-}
module Test.Subnet (
    test1,
    test2,
    test3
) where

import           Network.Basal.Protocols.Utils (findNIface, getDefaultNIface,
                                                host2ipv4)
import           Network.Basal.Subnet          (getSubnetMaskFromNIface, isin)
import qualified Test.TestUtils                as TT

import           Data.Maybe                    (isJust)
import           Network.Info                  (IPv4 (..), ipv4, name)
import           Test.HUnit                    ((@?=))

testmodule :: String -> TT.Assertion -> TT.Test
testmodule = TT.testmodule . (++) "Subnet."

test1 :: IO TT.Test
test1 = testmodule "test1 getSubnetMaskFromNIface: " . (@?=) True . isJust <$> (getSubnetMaskFromNIface =<< (name <$> getDefaultNIface))

type SrcNIC = String
type DstIp = String

test2int :: Bool -> SrcNIC -> DstIp -> IO TT.Test
test2int b src dst = (>>=) (findNIface src) $ maybe (return $ testmodule "test2 isin: " TT.fail) $ \nic -> do
        target <- host2ipv4 dst
        let (IPv4 ip) = ipv4 nic
            (IPv4 dstip) = target
        return $ testmodule "test2 isin: " $ maybe TT.fail ((@?=) b . isin ip dstip) =<< getSubnetMaskFromNIface (name nic)


test2 :: SrcNIC -> DstIp -> IO TT.Test
test2 = test2int True

test3 :: SrcNIC -> DstIp -> IO TT.Test
test3 = test2int False
