{-# OPTIONS_GHC -Wall #-}
module Network.Basal.Tools.Wol (
    socket,
    sendTo
) where

import           Network.Basal.Protocols.Utils (host2addr)

import           Control.Monad                 (void)
import           Data.Bool                     (bool)
import qualified Data.ByteString               as B
import           Data.List.Split               (splitOn)
import           Data.Tuple.Extra              (dupe, first, second)
import           Data.Word                     (Word8)
import qualified Network.Socket                as NS hiding (recv, recvFrom,
                                                      send, sendTo)
import qualified Network.Socket.ByteString     as SB
import           Numeric                       (readHex)

type MacAddr = String

udp :: NS.ProtocolNumber
udp = 17

-- | socket for WOL
socket :: IO NS.Socket
socket = NS.socket NS.AF_INET NS.Datagram udp

broadCast :: String
broadCast = "255.255.255.255"

macAddrParse :: MacAddr -> [Word8]
macAddrParse = uncurry id . first (bool (error "Invalid mac address")) . second ((==6) . length) . dupe . map (fst . head . readHex) . splitOn ":"

magicPacket :: MacAddr -> B.ByteString
magicPacket = B.pack . (++) prefix . concat . replicate 16 . macAddrParse
    where
        prefix = replicate 6 255

-- | Sending method for WOL
sendTo :: MacAddr -> NS.PortNumber -> NS.Socket -> IO ()
sendTo maddr port sock = do
    (NS.SockAddrInet _ ha) <- host2addr broadCast
    NS.setSocketOption sock NS.Broadcast 1
    void $ SB.sendTo sock (magicPacket maddr) $ NS.SockAddrInet port ha
