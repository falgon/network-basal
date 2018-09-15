{-# OPTIONS_GHC -Wall #-}
module Test.Protocols.IP.EtherHeader (
    etherHeader
) where

import qualified Test.Protocols.IP.TestUtils as TT
import qualified Test.Tools.Ether as TE

import qualified Network.Basal.Protocols.Utils as PU
import qualified Network.Basal.Protocols.IP.Internal as IP
import qualified Network.Basal.Protocols.Link.Ether as LE

import qualified Data.ByteString.Lazy as BL
import Test.HUnit ((@?=))

type IfaceName = String
type DstIp = String

testmodule :: String -> TT.Assertion -> TT.Test
testmodule = TT.testmodule . (++) "EtherHeader"

l2merr :: String
l2merr = "etherHeader.l2m: unexpected conversion"

etherHeader :: IfaceName -> DstIp -> IO TT.Test
etherHeader shost dhost = TE.findNIfaceWhenFailedErr testmodule "etherHeader: " shost $ \nic -> 
    TE.getDstMacAddrWhenFailedErr testmodule "etherHeader: " nic dhost $ \dstm -> 
         flip (maybe (return $ testmodule l2merr TT.fail)) (PU.l2m $ LE.ethDhost $ LE.fromHeader $ BL.toStrict $ IP.etherHeader dstm nic) $ \dhm ->
            return $ testmodule "etherHeader: " $ show dhm @?= show dstm
