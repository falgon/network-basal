{-# OPTIONS_GHC -Wall #-}
module Network.Basal.Tools.Icmp.Utils (
    dataSize,
    ICMPResult (..)
) where

import qualified Network.Basal.Protocols.IP.Icmp as ICMP (Header)
import qualified Network.Basal.Protocols.IP.Internal as IP (Header)

import Data.Int (Int64)
import Data.Time.Clock (NominalDiffTime)
import Data.Word (Word8)
import qualified Network.Socket as NS

dataSize :: Word8
dataSize = 56

data ICMPResult = ICMPResult { 
    dataLength :: Int64, 
    delay :: NominalDiffTime,
    sender :: Maybe NS.HostName,
    ipH :: IP.Header,
    icmpH :: ICMP.Header
} deriving Show
