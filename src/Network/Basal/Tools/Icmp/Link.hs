{-# OPTIONS_GHC -Wall #-}
module Network.Basal.Tools.Icmp.Link (
    module Network.Basal.Tools.Icmp.Utils,
    EchoParam (..),
    echo
) where

import qualified Network.Basal.Protocols.IP.Icmp.Identifiers as ICMP
import qualified Network.Basal.Protocols.IP.Icmp.Internal    as ICMP
import qualified Network.Basal.Protocols.IP.Internal         as IP
import qualified Network.Basal.Protocols.Link.Ether          as LE
import           Network.Basal.Tools.Icmp.Utils

import           Data.Bits                                   ((.&.))
import qualified Data.ByteString.Lazy                        as BL
import           Data.Time.Clock                             (NominalDiffTime,
                                                              diffUTCTime,
                                                              getCurrentTime)
import           Data.Word                                   (Word16)
import qualified Network.Info                                as NI
import           Network.Pcap                                (PcapHandle,
                                                              PktHdr)
import qualified Network.Socket                              as NS
import           System.Posix.Types                          (ProcessID)

data EchoParam = EchoParam {
    dst    :: NS.SockAddr,
    nic    :: NI.NetworkInterface,
    socket :: PcapHandle,
    pid    :: ProcessID,
    seq    :: Word16
}

sendEcho :: EchoParam -> IO (Maybe ((PktHdr, LE.Structure (IP.Structure ICMP.Structure)), NominalDiffTime))
sendEcho (EchoParam d n sock p s) = (>>=) (ICMP.ethPacket st n d) $ maybe (return Nothing) $ \packet -> do
    start <- getCurrentTime
    LE.send sock $ BL.toStrict packet
    res <- LE.recvFrom sock :: IO (PktHdr, LE.Structure (IP.Structure ICMP.Structure))
    stop <- getCurrentTime
    return $ Just (res, (*1000) $ realToFrac $ diffUTCTime stop start)
    where
        st = ICMP.Structure {
            ICMP.icmpH = ICMP.Header {
                ICMP.icmpType = ICMP.IcmpEcho,
                ICMP.icmpCode = 0,
                ICMP.icmpChecksum = 0,
                ICMP.icmpIdent = fromIntegral p,
                ICMP.icmpSeq = s
            },
            ICMP.icmpData = BL.pack $ map (.&. 255) [1..dataSize]
        }

-- | Send for ICMP Echo by Raw data
echo :: EchoParam -> IO (Maybe ICMPResult)
echo p = (>>=) (sendEcho p) $ maybe (return Nothing) $ \((_, rst), dl) -> do
    (hname, _) <- NS.getNameInfo [] True False $ NS.SockAddrInet 0 $ IP.ipSaddr $ IP.ipH $ LE.ethData rst
    if ICMP.IcmpEchoReply == ICMP.icmpType (ICMP.icmpH $ IP.ipData $ LE.ethData rst) && fromIntegral (pid p) == ICMP.icmpIdent (ICMP.icmpH $ IP.ipData $ LE.ethData rst) then
        return $ Just $ ICMPResult (fromIntegral (fromIntegral (IP.ipTotLen $ IP.ipH $ LE.ethData rst) - ICMP.icmpLength)) dl hname (IP.ipH $ LE.ethData rst) (ICMP.icmpH $ IP.ipData $ LE.ethData rst)
    else return Nothing

