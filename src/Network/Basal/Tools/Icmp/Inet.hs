{-# OPTIONS_GHC -Wall #-}
module Network.Basal.Tools.Icmp.Inet (
    module Network.Basal.Tools.Icmp.Utils,
    echo
) where

import Network.Basal.Tools.Icmp.Utils
import qualified Network.Basal.Protocols.Link.Ether as LE
import qualified Network.Basal.Protocols.IP.Internal as IP
import qualified Network.Basal.Protocols.IP.Icmp.Internal as ICMP
import qualified Network.Basal.Protocols.IP.Icmp.Identifiers as ICMP
import Network.Basal.Protocols.Utils (host2addr)

import Control.Monad (void)
import Data.Bits ((.&.))
import Data.Word (Word16)
import Data.Time.Clock (getCurrentTime, diffUTCTime, NominalDiffTime)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Network.Socket hiding (recvFrom, sendTo)
import System.Posix.Types (ProcessID)

sendAsINET :: SockAddr -> Socket -> Word16 -> Word16 -> BL.ByteString -> IO ((LE.Packet, SockAddr), NominalDiffTime)
sendAsINET addr sock ident se icmpdata = do
    start <- getCurrentTime
    void $ ICMP.sendToINET sock p addr
    received <- ICMP.recvFromINET sock
    stop <- getCurrentTime
    return (received, (*1000) $ realToFrac $ diffUTCTime stop start)
    where
        p = B.concat $ BL.toChunks $ ICMP.icmpPacket ICMP.Structure { 
            ICMP.icmpH = ICMP.Header { 
                ICMP.icmpType = ICMP.IcmpEcho,
                ICMP.icmpCode = 0,
                ICMP.icmpChecksum = 0,
                ICMP.icmpIdent = ident,
                ICMP.icmpSeq = se
            }, 
            ICMP.icmpData = icmpdata 
        }

sendEcho :: (SockAddr -> Socket -> Word16 -> Word16 -> BL.ByteString -> IO ((LE.Packet, SockAddr), NominalDiffTime)) -> SockAddr -> Socket -> Word16 -> Word16 -> BL.ByteString -> IO (Maybe ICMPResult)
sendEcho sendas addr sock ident se icmpdata = do
    ((receivedPacket, address), dl) <- sendas addr sock ident se icmpdata
    let ips = LE.fromData receivedPacket :: IP.Structure ICMP.Structure
    (hname, _) <- getNameInfo [] True False address 
    if LE.checkReply ips && ICMP.IcmpEchoReply == ICMP.icmpType (ICMP.icmpH $ IP.ipData ips) && ident == ICMP.icmpIdent (ICMP.icmpH $ IP.ipData ips) then return $ Just $ ICMPResult 64 dl hname (IP.ipH ips) (ICMP.icmpH $ IP.ipData ips) else return Nothing

-- | Send for ICMP Echo by AF_INET socket
echo :: String -> Socket -> ProcessID -> Word16 -> IO (Maybe ICMPResult)
echo hostname sock pid se = host2addr hostname >>= flip (flip (flip (flip (sendEcho sendAsINET) sock) $ fromIntegral pid) se) (BL.pack $ map (.&. 255) [1..dataSize])
