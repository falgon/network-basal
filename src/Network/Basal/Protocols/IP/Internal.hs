{-# OPTIONS_GHC -Wall #-}
module Network.Basal.Protocols.IP.Internal (
    ver, ihl, verihl,
    Header (..),
    fromHeader,
    Structure (..),
    checksum,
    IpProtocol (..),
    IpProtocolInfo (..),
    etherHeader,
    ipHeader,
    lookupDGW,
    getDGWMacAddr
) where

import qualified Network.Basal.Protocols.Link.Ether as LE
import qualified Network.Basal.Tools.Arp as ARP
import qualified Network.Basal.Protocols.Utils as PU
import Network.Basal.Protocols.IP.Identifiers

import Control.Applicative ((<|>))
import Control.Monad.Fix (fix)
import qualified Data.Binary.Get as BG
import qualified Data.Binary.Put as BP
import Data.Bits (shiftL, shiftR, complement, (.&.), (.|.))
import Data.Bool (bool)
import qualified Data.ByteString.Lazy as BL (length, snoc, ByteString, fromStrict, splitAt, take, drop)
import qualified Data.ByteString as BS
import Data.List (foldl')
import Data.List.Split (chunksOf)
import Data.Int (Int64)
import Data.Maybe (maybe)
import Data.Tuple.Extra (first, second, (&&&))
import Data.Word (Word8, Word16, Word32)
import qualified Network.Socket as NS hiding (send, sendTo, recv, recvFrom)
import qualified Network.Info as NI
import System.IO (withFile, hGetLine, hIsEOF, IOMode (..))

newtype IpVI = IpVI Word8 deriving (Eq, Ord, Bounded, Read)

instance Show IpVI where
    show = uncurry (++) . first ((++) "{Version = " . show) . second ((++) ", Ihl = " . flip (++) "}". show) . verihl

{-# INLINE ver #-}
ver :: IpVI -> Word8
ver (IpVI d) = d `shiftR` 4 

{-# INLINE ihl #-}
ihl :: IpVI -> Word8
ihl (IpVI d) = d .&. 0x0f

{-# INLINE verihl #-}
verihl :: IpVI -> (Word8, Word8)
verihl = ver &&& ihl

data Header = Header {
    ipVI :: IpVI, -- version and Ihl.
    ipTos :: Word8,
    ipTotLen :: Word16,
    ipId :: Word16,
    ipFragOff :: Word16,
    ipTtl :: Word8,
    ipProtocol :: ProtocolNum,
    ipCheck :: Word16,
    ipSaddr :: Word32,
    ipDaddr :: Word32
} 

instance Show Header where
    show (Header vi tos tot id' foff ttl pro chk saddr daddr) = "Header {ipVI = " ++ show vi ++
        ", ipTos = " ++ show tos ++
        ", ipTotLen = " ++ show tot ++
        ", ipId = " ++ show id' ++
        ", ipFragOff = " ++ show foff ++
        ", ipTtl = " ++ show ttl ++
        ", ipProtocol = " ++ show pro ++
        ", ipCheck = " ++ show (PU.int2Hex chk) ++ 
        ", ipSaddr = " ++ show (NI.IPv4 saddr) ++
        ", ipDaddr = " ++ show (NI.IPv4 daddr) ++ "}"

fromHeader :: BL.ByteString -> Header
fromHeader = BG.runGet f
    where
        f = do
            ipvi <- BG.getWord8 
            tos <- BG.getWord8
            tot <- BG.getWord16be
            id' <- BG.getWord16be
            foff <- BG.getWord16be
            ttl <- BG.getWord8
            pa <- BG.getWord8
            c <- BG.getWord16be
            sa <- BG.getWord32le
            da <- BG.getWord32le
            return $ Header (IpVI ipvi) tos tot id' foff ttl (toEnum $ fromIntegral pa) c sa da

class IpProtocol a where
    fromIpData :: BL.ByteString -> a
    pack :: a -> BL.ByteString 
    checkProtoReply :: a -> Bool

data Structure a = Structure {
    ipH :: Header,
    ipData :: a,
    ipOption :: BL.ByteString
} deriving Show

getVerIhl :: BS.ByteString -> (Word8, Word8)
getVerIhl = BG.runGet ((flip shiftR 4 &&& (.&. 0x0f)) <$> BG.getWord8) . BL.fromStrict

instance IpProtocol a => LE.EtherData (Structure a) where
    fromData p = Structure (({-BG.runGet getHeader-}fromHeader iph) { ipVI = IpVI $ (v `shiftL` 4) .|. i }) (fromIpData (BL.drop optLen ipdWithOpt)) $ BL.take optLen ipdWithOpt -- drop and take the option data.
        where
            defHdrLen = fromIntegral (noOptHeaderLength * 4) :: Int64
            (v, i) = getVerIhl p
            optLen = (fromIntegral i :: Int64) * 4 - defHdrLen
            (iph, ipdWithOpt) = BL.splitAt (fromIntegral defHdrLen) $ BL.fromStrict p
    checkReply st = (r == 0 || r == 0xffff) && (ver (ipVI $ ipH st) == 4) && checkProtoReply (ipData st)
        where
            pkd = pack $ ipData st
            r = if BL.length (ipOption st) == 0 then checksum pkd else checksum $ ipOption st <> pkd

data IpProtocolInfo = IpProtocolInfo {
    len :: Word16,
    pnum :: ProtocolNum
}

-- | checksum
checksum :: BL.ByteString -> Word16
checksum bs = complement $ fromIntegral total + fromIntegral (total `shiftR` 16)
    where
        w16 = bool ((:) <$> BG.getWord16be <*> w16) (return []) =<< BG.isEmpty
        ws = BG.runGet w16 $ if even (BL.length bs) then bs else BL.snoc bs 0
        total = foldl' (flip (.) fromIntegral . (+)) 0 ws :: Word32

etherHeader :: NI.MAC -> NI.NetworkInterface -> BL.ByteString
etherHeader dm ni = LE.etherHeader (LE.Header (PU.m2l dm) (PU.m2l $ NI.mac ni) LE.EthIP)

ipHeader :: IpProtocolInfo -> NI.NetworkInterface -> NS.HostAddress -> BL.ByteString
ipHeader proto ni daddr = packData $ g $ checksum $ packData $ g 0
    where
        (NI.IPv4 saddr) = NI.ipv4 ni
        g c = Header { 
            ipVI = IpVI $ (4 `shiftL` 4) .|. noOptHeaderLength,
            ipTos = 0,
            ipTotLen = len proto + 64,
            ipId = 0,
            ipFragOff = 0,
            ipTtl = defTTL,
            ipProtocol = pnum proto,
            ipCheck = c,
            ipSaddr = saddr,
            ipDaddr = daddr
        }
        packData = BP.runPut . f
            where
                f :: Header -> BP.Put
                f (Header (IpVI vh) tos tot id' foff ttl pa c sa da) = BP.putWord8 vh *>
                    BP.putWord8 tos                 *>
                    BP.putWord16be tot              *>
                    BP.putWord16be id'              *>
                    BP.putWord16be foff             *>
                    BP.putWord8 ttl                 *>
                    BP.putWord8 (LE.paramVal pa)    *>
                    BP.putWord16be c                *>
                    BP.putWord32le sa               *>
                    BP.putWord32le da

-- | Lookup from /proc/net/route, it looks like this:
-- Iface   Destination     Gateway         Flags   RefCnt  Use     Metric  Mask            MTU     Window  IRTT               
-- eth0    00000000        0202000A        0003    0       0       100     00000000        0       0       0                  
-- eth0    0002000A        00000000        0001    0       0       0       00FFFFFF        0       0       0 
lookupDGW :: IO (Maybe NS.HostAddress)
lookupDGW = withFile "/proc/net/route" ReadMode $ \h -> fix $ \f -> do
    iseof <- hIsEOF h
    if iseof then return Nothing
    else do
        l <- words <$> hGetLine h
        if length l > 10 && l !! 1 == "00000000" then return $ l2i (foldl' (flip (.) (fromIntegral . PU.hex2Int) . flip (:)) [] $ chunksOf 2 (l !! 2)) <|> Nothing 
        else f
            where
                l2i [i,p,a,d] = Just $ NS.tupleToHostAddress (i, p, a, d)
                l2i _ = Nothing

-- | Get Default gateway's MAC address by cache or ARP request.
getDGWMacAddr :: NI.NetworkInterface -> Int64 -> IO (Maybe NI.MAC)
getDGWMacAddr nic timeout = maybe (return Nothing) (flip (ARP.getMacAddr nic) timeout . NS.SockAddrInet 0) =<< lookupDGW 
