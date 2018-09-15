{-# OPTIONS_GHC -Wall #-}
module Network.Basal.Protocols.IP.Icmp.Internal (
    Header (..),
    Structure (..),
    recvMax,
    icmpLength,
    socket,
    socketINET,
    sendToINET,
    recvFromINET,
    icmpPacket,
    ipPacket,
    ethPacket
) where

import Network.Basal.Protocols.IP.Icmp.Identifiers
import qualified Network.Basal.Tools.Ether as TE
import qualified Network.Basal.Protocols.Link.Ether as LE
import qualified Network.Basal.Protocols.IP.Identifiers as II
import qualified Network.Basal.Protocols.IP.Internal as II

import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Tuple.Extra (second, dupe)
import Data.Int (Int64)
import Data.Word (Word8, Word16)
import Network.Pcap (PcapHandle, setFilter, openLive)
import qualified Network.Socket as NS hiding (recv, recvFrom, send, sendTo)
import Network.Socket.ByteString as SB
import qualified Network.Info as NI

recvMax :: Int
recvMax = 2048

icmpLength :: Word16
icmpLength = 20

data Header = Header {
    icmpType :: IcmpType,
    icmpCode :: Word8,
    icmpChecksum :: Word16,
    icmpIdent :: Word16,
    icmpSeq :: Word16
} deriving Show

data Structure = Structure {
    icmpH :: Header,
    icmpData :: BL.ByteString
} deriving Show

putIcmp :: Structure -> BP.Put
putIcmp (Structure (Header t c cs ide se) d) = BP.putWord8 (LE.paramVal t) *>
    BP.putWord8 c       *>
    BP.putWord16be cs   *>
    BP.putWord16be ide  *>
    BP.putWord16be se   *>
    BP.putLazyByteString d

instance II.IpProtocol Structure where
    fromIpData p = Structure (BG.runGet getHeader icmph) icmpd
        where
            (icmph, icmpd) = BL.splitAt 8 p
            getHeader = do
                i <- BG.getWord8
                code <- BG.getWord8
                cs <- BG.getWord16be
                ide <- BG.getWord16be
                s <- BG.getWord16be
                return $ Header (toEnum $ fromIntegral i) code cs ide s
    checkProtoReply st = c == 0 || c == 0xffff
        where
            c = II.checksum (II.pack st)
    pack = BP.runPut . putIcmp

socket :: String -> Int64 -> IO PcapHandle
socket = ((=<<) (return . fst . second (flip (flip (`setFilter` "icmp") True) 0) . dupe) .) . flip (`openLive` recvMax) False

socketINET :: IO NS.Socket
socketINET = NS.socket NS.AF_INET NS.Raw $ fromIntegral $ fromEnum II.Icmp

sendToINET :: NS.Socket -> BS.ByteString -> NS.SockAddr -> IO Int
sendToINET = SB.sendTo

recvFromINET :: NS.Socket -> IO (BS.ByteString, NS.SockAddr)
recvFromINET = flip SB.recvFrom recvMax

icmpPacket :: Structure -> BL.ByteString
icmpPacket (Structure (Header pn t _ ident sq) icd) = f $ II.checksum $ f 0
    where
        f c = BP.runPut $ putIcmp $ Structure (Header pn t c ident sq) icd

ipPacket :: Structure -> NI.NetworkInterface -> NS.SockAddr -> BL.ByteString
ipPacket st nic (NS.SockAddrInet _ addr) = II.ipHeader (II.IpProtocolInfo icmpLength II.Icmp) nic addr <> icmpPacket st
ipPacket _ _ _ = error "sorry, implemented only ipv4 yet."

ethPacket :: Structure -> NI.NetworkInterface -> NS.SockAddr -> IO (Maybe BL.ByteString)
ethPacket st nic (NS.SockAddrInet p addr) = (>>=) (TE.getDstMacAddr nic $ NS.SockAddrInet p addr) $ maybe (return Nothing) $ \dstm -> 
    return $ Just $ II.etherHeader dstm nic <> ipPacket st nic (NS.SockAddrInet p addr)
ethPacket _ _ _ = error "sorry, implemented only ipv4 yet."
