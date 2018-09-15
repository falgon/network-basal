{-|
Module      : Network.Basal.Protocols.Link.Arp.Internal
Description : Utilities of the arp packet. 
Copyright   : (C) Roki, 2018
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
{-# OPTIONS_GHC -Wall #-}

module Network.Basal.Protocols.Link.Arp.Internal (
    socket,
    socket',
    Header (..),
    Structure (..),
    arpPacket,
    arpPacket',
    ethPacket,
    ethPacket',
    arpPacketWithDefaultNIC,
    arpPacketWithDefaultNIC',
    ethPacketWithDefaultNIC,
    ethPacketWithDefaultNIC',
    lookupATP,
    lookupArpTable
) where

import qualified Network.Basal.Protocols.Utils as PU
import Network.Basal.Protocols.Link.Arp.Parameters
import qualified Network.Basal.Protocols.Link.Ether as LE
import Network.Basal.Protocols.Utils (findNIface, getDefaultNIface, hex2Int, host2addr)

import Control.Applicative ((<|>))
import Control.Monad (mapM_, replicateM, replicateM_, (>=>), liftM2)
import Control.Monad.Fix (fix)
import Data.Bits (finiteBitSize, (.&.), shiftR)
import Data.Bool (bool)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Word (Word8, Word32)
import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import Data.List (unfoldr)
import Data.List.Split (wordsBy)
import Data.Tuple.Extra (second, dupe)
import Data.Int (Int64)
import Data.Monoid ((<>))
import System.IO (withFile, IOMode (..), hIsEOF, hGetLine)
import qualified Network.Socket as NS
import qualified Network.Info as NI (ipv4, mac, MAC (..), NetworkInterface, IPv4 (..), name)
import Network.Pcap (openLive, setFilter, PcapHandle)

data Header = Header {
    arpHrd :: HardwareType,
    arpPro :: LE.EtherType,
    arpHln :: Word8,
    arpPln :: Word8,
    arpOp :: OperationCode
} deriving Show


data Structure = Structure {
    arpH :: Header,
    arpSenderMac :: [Word8],
    arpSenderIp :: Word32,
    arpTargetMac :: [Word8],
    arpTargetIp :: Word32
}

instance Show Structure where
    show (Structure h sm si tm ti) = "Structure {arpH = " ++ show h ++ 
        ", arpSenderMac = " ++ show sm ++ 
        ", arpSenderIp = " ++ concat (f si) ++ 
        ", arpTargetMac = " ++ show tm ++ 
        ", arpTargetIp = " ++ concat (f ti) ++
        "}"
        where
            f = unfoldr (\(x, i) -> if x /= 0 then Just (g x i, (x `shiftR` 8, succ i)) else Nothing) . flip (,) (0 :: Int)
            g = flip (.) (bool [] "." . (/=3)) . (++) . show . (.&.) 0x000000ff

instance LE.EtherData Structure where
    fromData = BG.runGet f . BL.fromStrict
        where
            f :: BG.Get Structure
            f = do
                arphrd <- BG.getWord16be
                arppro <- BG.getWord16be
                arphln <- BG.getWord8
                arppln <- BG.getWord8
                arpop <- BG.getWord16be
                sendermac <- replicateM 6 BG.getWord8 
                senderip <- BG.getWord32le
                targetmac <- replicateM 6 BG.getWord8
                targetip <- BG.getWord32le
                return $ Structure (Header (toEnum $ fromIntegral arphrd) (toEnum $ fromIntegral arppro) arphln arppln (toEnum $ fromIntegral arpop)) sendermac senderip targetmac targetip
    checkReply = (OpcRep==) . arpOp . arpH

-- | socket for ARP
socket' :: String -> Int64 -> IO PcapHandle
socket' = ((=<<) (return . fst . second (flip (flip (`setFilter` "arp") True) 0) . dupe) .) . flip (`openLive` 64) False

socket :: NI.NetworkInterface -> Int64 -> IO PcapHandle
socket = socket' . NI.name

-- | Generate raw Ether header data set for arp packet and return it
etherHeader :: NI.NetworkInterface -> BL.ByteString
etherHeader ni = LE.etherHeader (LE.Header (replicate (fromIntegral LE.ethAlen) 0xff) (PU.m2l $ NI.mac ni) LE.EthArp)

-- | Generate raw arp packet data and return it
arpPacket :: NI.NetworkInterface -> NS.SockAddr -> BL.ByteString
arpPacket ni addr = do
    let (NI.IPv4 ipv4) = NI.ipv4 ni
        (NS.SockAddrInet _ target) = addr
    packData Structure {
        arpH = Header HrdtEthernet LE.EthIP LE.ethAlen (fromIntegral (finiteBitSize ipv4) `div` 8) OpcReq,
        arpSenderMac = PU.m2l $ NI.mac ni,
        arpSenderIp = ipv4,
        arpTargetMac = replicate (fromIntegral LE.ethAlen) 0,
        arpTargetIp = target
    }
    where
        packData = BP.runPut . f
            where
                f :: Structure -> BP.Put
                f (Structure (Header hwtype prototype hwsize protosize opcode) sendermac senderip targetmac targetip) = BP.putWord16be (LE.paramVal hwtype) *>
                    BP.putWord16be (LE.paramVal prototype)  *>
                    BP.putWord8 hwsize                      *>
                    BP.putWord8 protosize                   *>
                    BP.putWord16be (LE.paramVal opcode)     *>
                    mapM_ BP.putWord8 sendermac             *>
                    BP.putWord32le senderip                 *>
                    mapM_ BP.putWord8 targetmac             *>
                    BP.putWord32le targetip                 *>
                    replicateM_ 2 (BP.putWord64be 0) >> BP.putWord16be 0 -- padding

-- | Same as arpPacket. It takes two arguments as a String.
arpPacket' :: String -> String -> IO (Maybe BL.ByteString)
arpPacket' srcInf targetHost = findNIface srcInf >>= maybe (return Nothing) (flip id (host2addr targetHost) . fmap . (.) Just . arpPacket)

-- | Detect the default NIC, generate arp packet of name based on it, and return it.
arpPacketWithDefaultNIC :: NS.SockAddr -> IO BL.ByteString
arpPacketWithDefaultNIC = flip id getDefaultNIface . fmap . flip arpPacket

-- | Same as arpPacketWithDefaultNIC. It takes a argument as a String.
arpPacketWithDefaultNIC' :: String -> IO BL.ByteString
arpPacketWithDefaultNIC' = liftM2 arpPacket getDefaultNIface . host2addr

-- | Generate complete raw Ethernet packet packed (ether header and arp packet) and return it.
ethPacket :: NI.NetworkInterface -> NS.SockAddr -> BS.ByteString
ethPacket nif targetHost = BL.toStrict $ etherHeader nif <> arpPacket nif targetHost

-- | Same as ethPacket. It takes two arguments as a String.
ethPacket' :: String -> String -> IO (Maybe BS.ByteString)
ethPacket' nif targetHost = findNIface nif >>= maybe (return Nothing) (flip id (host2addr targetHost) . fmap . (.) Just . ethPacket)

-- | Detect the default NIC, generate complete raw Ethernet packet packed (ether header and arp packet) and return it.
ethPacketWithDefaultNIC :: NS.SockAddr -> IO BS.ByteString
ethPacketWithDefaultNIC addr = getDefaultNIface >>= fmap BL.toStrict . flip id (arpPacketWithDefaultNIC addr) . fmap . (<>) . etherHeader

-- | Same as ethPacketWithDefaultNIC. It takes a argument as a String.
ethPacketWithDefaultNIC' :: String -> IO BS.ByteString
ethPacketWithDefaultNIC' = host2addr >=> ethPacketWithDefaultNIC

lookupATP :: NI.IPv4 -> String -> IO (Maybe NI.MAC)
lookupATP ip path = withFile path ReadMode $ \h -> do
    let (NI.IPv4 ipv4) = ip
    ipa <- NS.inet_ntoa ipv4
    fix $ \f -> do
        iseof <- hIsEOF h
        if iseof then return Nothing
        else do
            l <- words <$> hGetLine h
            if not (null l) && head l == ipa then return $ PU.l2m (map (fromIntegral . hex2Int) $ wordsBy (==':') $ l !! 3) <|> Nothing else f

-- | Lookup from /proc/net/arp, it looks like this:
-- IP address       HW type     Flags       HW address            Mask     Device
-- 192.168.12.2     0x1         0x2         xx:xx:xx:xx:xx:xx     *        eth0
-- 192.168.12.3     0x1         0x2         xx:xx:xx:xx:xx:xx     *        eth0
--
lookupArpTable :: NI.IPv4 -> IO (Maybe NI.MAC)
lookupArpTable = flip lookupATP "/proc/net/arp"
