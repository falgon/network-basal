{-# OPTIONS_GHC #-}

module Test.Tools.Ether (
    getDstMacAddrSameSubnet,
    getDstMacAddrDiffSubnet,
    findNIfaceWhenFailedErr,
    getMacAddrWhenFailedErr,
    getDstMacAddrWhenFailedErr
) where

import qualified Test.Tools.TestUtils as TT
import qualified Network.Basal.Protocols.Utils as PU
import qualified Network.Basal.Protocols.IP.Internal as IP
import qualified Network.Basal.Tools.Ether as TE 
import qualified Network.Basal.Tools.Arp as ARP

import Data.Int (Int64)
import qualified Network.Info as NI
import qualified Network.Socket as NS
import Test.HUnit ((@?=))

testmodule :: String -> TT.Assertion -> TT.Test
testmodule = TT.testmodule . (++) "Ether."

type IfaceName = String
type DstIp = String

timeout :: Int64
timeout = 5*1000*1000

findnifaceerr :: (String -> TT.Assertion -> TT.Test) -> String -> TT.Test
findnifaceerr tmod s = tmod (s ++ ".findNIface: failed to find the specified network interface.") TT.fail

getarperr :: (String -> TT.Assertion -> TT.Test) -> String -> TT.Test
getarperr tmod s = tmod (s ++ ".getMacAddr: failed to get the MAC address from specified ip address.") TT.fail

-- flip testmodule TT.fail . flip (++) ".getMacAddr: failed to get the MAC address from specified ip address."

getdgwerr :: String -> TT.Test
getdgwerr = flip testmodule TT.fail . 
    flip (++) ".getDGWMacAddr: failed to get the MAC address of the specified default gateway."

getdstmacaddrerr :: (String -> TT.Assertion -> TT.Test) -> String -> TT.Test
getdstmacaddrerr tmod s = tmod (s ++ ".getDstMacAddr: failed to get destination MAC address.") TT.fail

findNIfaceWhenFailedErr :: (String -> TT.Assertion -> TT.Test) -> String -> IfaceName -> (NI.NetworkInterface -> IO TT.Test) -> IO TT.Test
findNIfaceWhenFailedErr tmod s iface = (>>=) (PU.findNIface iface) . maybe (return $ findnifaceerr tmod s)

getMacAddrWhenFailedErr :: (String -> TT.Assertion -> TT.Test) -> String -> NI.NetworkInterface -> DstIp -> Int64 -> (NI.MAC -> IO TT.Test) -> IO TT.Test
getMacAddrWhenFailedErr tmod s iface host timeout = (>>=) (PU.host2addr host >>= flip (ARP.getMacAddr iface) timeout) . maybe (return $ getarperr tmod s)

getDstMacAddrWhenFailedErr :: (String -> TT.Assertion -> TT.Test) -> String -> NI.NetworkInterface -> DstIp -> (NI.MAC -> IO TT.Test) -> IO TT.Test
getDstMacAddrWhenFailedErr tmod s iface dstip = (>>=) (PU.host2addr dstip >>= TE.getDstMacAddr iface) . maybe (return $ getdstmacaddrerr tmod s)

getDstMacAddrSameSubnet :: IfaceName -> DstIp -> IO TT.Test
getDstMacAddrSameSubnet iface dst = findNIfaceWhenFailedErr testmodule "getDstMacAddrSameSubnet: " iface $ \nic -> 
    getMacAddrWhenFailedErr testmodule "getDstMacAddrSameSubnet: " nic dst timeout $ \dstm -> 
        getDstMacAddrWhenFailedErr testmodule "getDstMacAddrSameSubnet: " nic dst $ return . testmodule "getDstMacAddr: " . (@?=) dstm

getDstMacAddrDiffSubnet :: IfaceName -> DstIp -> IO TT.Test
getDstMacAddrDiffSubnet iface dst = findNIfaceWhenFailedErr testmodule "getDstMacAddrDiffSubnet: " iface $ \nic ->
    (>>=) (IP.getDGWMacAddr nic timeout) $ maybe (return $ getdgwerr "getDstMacAddrDiffSubnet: ") $ \dgw ->
        getDstMacAddrWhenFailedErr testmodule "getDstMacAddrDiffSubnet: " nic dst $ return . testmodule "getDstMacAddr: " . (@?=) dgw
        
