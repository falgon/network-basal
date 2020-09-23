{-|
Module      : Network.Basal.Protocols.Link.Ether
Description : Utilities of ether header. Only DIX is supported, IEEE802.3, IEEE802.3+802.2(LLC) and IEEE802.2(LLC+SNAP) are not supported.
Copyright   : (C) Roki, 2018
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
{-# OPTIONS_GHC -Wall #-}

module Network.Basal.Protocols.Link.Ether (
    ethAlen,
    Packet,
    checkReply,
    EtherData,
    fromData,
    send,
    recvFrom,
    fromHeader,
    fromPacket,
    Header (..),
    Structure (..),
    EtherPacket,
    putHeader,
    etherHeader,
    EtherType (..),
    Parameter,
    paramVal
) where

import           Network.Basal.Protocols.Utils (int2Hex, l2m, predError,
                                                succError, toEnumError)

import           Control.Monad                 (replicateM)
import           Control.Monad.Fix             (fix)
import qualified Data.Binary.Get               as BG
import qualified Data.Binary.Put               as BP
import qualified Data.ByteString               as B
import qualified Data.ByteString               as BS (ByteString, splitAt)
import qualified Data.ByteString.Lazy          as BL (ByteString, fromStrict)
import           Data.Maybe                    (fromJust, fromMaybe)
import           Data.Tuple                    (swap)
import           Data.Word                     (Word8)
import           Network.Pcap                  (PcapHandle, PktHdr, nextBS,
                                                sendPacketBS)

ethAlen :: Word8
ethAlen = 6

class (Enum a) => Parameter a where
    paramVal :: Integral b => a -> b
    paramVal = fromIntegral . fromEnum

data EtherType =
    EthPup      |
    EthSprite   |
    EthIP       |
    EthArp      |
    EthRevArp   |
    EthAt       |
    EthAArp     |
    EthVlan     |
    EthIpx      |
    EthIpv6     |
    EthLoopBack
    deriving (Bounded, Eq, Ord, Read)

instance Show EtherType where
    show = (++) "0x" . int2Hex . fromEnum

instance Parameter EtherType

table :: [(EtherType, Int)]
table = [
    (EthPup, 0x0200),
    (EthSprite, 0x0500),
    (EthIP, 0x0800),
    (EthArp, 0x0806),
    (EthRevArp, 0x8035),
    (EthAt, 0x809b),
    (EthAArp, 0x80f3),
    (EthVlan, 0x8100),
    (EthIpx, 0x8137),
    (EthIpv6, 0x86dd),
    (EthLoopBack, 0x9000)]

instance Enum EtherType where
    succ EthPup      = EthSprite
    succ EthSprite   = EthIP
    succ EthIP       = EthArp
    succ EthArp      = EthRevArp
    succ EthRevArp   = EthAt
    succ EthAt       = EthAArp
    succ EthAArp     = EthVlan
    succ EthVlan     = EthIpx
    succ EthIpx      = EthIpv6
    succ EthIpv6     = EthLoopBack
    succ EthLoopBack = succError "EtherType"

    pred EthPup      = predError "EtherType"
    pred EthSprite   = EthPup
    pred EthIP       = EthSprite
    pred EthArp      = EthIP
    pred EthRevArp   = EthArp
    pred EthAt       = EthRevArp
    pred EthAArp     = EthAt
    pred EthVlan     = EthAArp
    pred EthIpx      = EthVlan
    pred EthIpv6     = EthIpx
    pred EthLoopBack = EthIpv6

    fromEnum = fromJust . flip lookup table
    toEnum i = fromMaybe (toEnumError "EtherType" i (minBound :: EtherType, maxBound :: EtherType)) $ lookup i (map swap table)

type EtherPacket = BS.ByteString

class EtherData a where
    fromData :: EtherPacket -> a
    checkReply :: a -> Bool

data Header = Header {
    ethDhost :: [Word8],
    ethShost :: [Word8],
    ethType  :: EtherType
}

instance Show Header where
    show (Header d s t) = "Header {ethDhost = " ++ show (fromJust (l2m d)) ++
        ", ethShost = " ++ show (fromJust (l2m s)) ++ ", ethType = " ++ show t ++ "}"

data Structure a = Structure {
    ethHeader :: Header,
    ethData   :: a
} deriving Show

type Packet = B.ByteString

send :: PcapHandle -> BS.ByteString -> IO ()
send = sendPacketBS

recvFrom :: (EtherData a) => PcapHandle -> IO (PktHdr, Structure a)
recvFrom p = fix $ \f -> do
    (hdr, raw) <- nextBS p
    let eth = fromPacket raw
        st = fromData $ ethData eth
    if checkReply st then return (hdr, Structure (ethHeader eth) st) else f

fromHeader :: Packet -> Header
fromHeader = BG.runGet (Header <$> readHost <*> readHost <*> (toEnum . fromIntegral <$> BG.getWord16be)) . BL.fromStrict
    where
        readHost = replicateM (fromIntegral ethAlen) BG.getWord8

fromPacket :: Packet -> Structure EtherPacket
fromPacket p = Structure (fromHeader eth) etd
    where
        (eth, etd) = BS.splitAt (fromIntegral $ ethAlen * 2 + 2) p -- ether_dhost + ether_shost + ether_type

putHeader :: Header -> BP.Put
putHeader (Header dhost shost ttype) = mapM_ (mapM_ BP.putWord8) [dhost, shost] >> BP.putWord16be (paramVal ttype)

etherHeader :: Header -> BL.ByteString
etherHeader = BP.runPut . putHeader
