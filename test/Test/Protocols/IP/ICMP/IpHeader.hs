module Test.Protocols.IP.ICMP.IpHeader (
    ipHeader
) where

import qualified Test.Protocols.IP.TestUtils as TT
import qualified Test.Tools.Ether as TE

import qualified Network.Basal.Protocols.IP.Internal as IP
import qualified Network.Basal.Protocols.IP.Identifiers as IP
import qualified Network.Basal.Protocols.Utils as PU

import qualified Data.ByteString.Lazy as BL
import qualified Network.Info as NI
import Test.HUnit ((@?=))

import Data.List (unfoldr)
import Data.Bits

type IfaceName = String
type DstIp = String

testmodule :: String -> TT.Assertion -> TT.Test
testmodule = TT.testmodule . (++) "IpHeader"

ipHeader :: IfaceName -> DstIp -> IO TT.Test
ipHeader shost dhost = TE.findNIfaceWhenFailedErr testmodule "ipHeader: " shost $ \nic -> do
    addr <- PU.host2ipv4 dhost
    let (NI.IPv4 ip) = addr
    let h = IP.fromHeader $ IP.ipHeader (IP.IpProtocolInfo 8 IP.Icmp) nic ip
    return $ testmodule "ipHeader: " $ True @?= True
