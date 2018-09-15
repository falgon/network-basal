{-# OPTIONS_GHC -Wall #-}
module Main where

import Control.Monad (sequence)

import Test (runTest)
import Test.HUnit (Test (..))

import qualified Test.Protocols.Link.Arp.LookupTable as TAL (test)
import qualified Test.Protocols.IP.LookupDGW as TIL (test1, test2)
import qualified Test.Protocols.IP.GetDGWMacAddr as TIG (test1, test2)
import qualified Test.Subnet as TS (test1, test2, test3)
import qualified Test.Tools.Ether as TE (getDstMacAddrSameSubnet, getDstMacAddrDiffSubnet)
import qualified Test.Protocols.IP.EtherHeader as IE (etherHeader)
import qualified Test.Protocols.IP.ICMP.IpHeader as II (ipHeader)

mynic :: String
mynic = "eth2"

sameSubnetPartnerIp :: String
sameSubnetPartnerIp = "192.168.33.12"

diffSubnetPartnerIp :: String
diffSubnetPartnerIp = "8.8.8.8"

main :: IO ()
main = sequence [
    TAL.test, 
    TIL.test1, 
    TIL.test2, 
    TIG.test1, 
    TIG.test2, 
    TS.test1, 
    TS.test2 mynic sameSubnetPartnerIp,
    TS.test3 mynic diffSubnetPartnerIp,
    TE.getDstMacAddrSameSubnet mynic sameSubnetPartnerIp,
    TE.getDstMacAddrDiffSubnet mynic diffSubnetPartnerIp,
    IE.etherHeader mynic sameSubnetPartnerIp,
    IE.etherHeader mynic diffSubnetPartnerIp,
    II.ipHeader mynic sameSubnetPartnerIp
    ] >>= runTest . TestList
