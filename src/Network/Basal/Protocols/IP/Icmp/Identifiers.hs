{-# OPTIONS_GHC -Wall #-}
module Network.Basal.Protocols.IP.Icmp.Identifiers (
    IcmpType (..)
) where

import           Network.Basal.Protocols.Link.Ether (Parameter)
import           Network.Basal.Protocols.Utils      (int2Hex, predError,
                                                     succError)

import           Data.Maybe                         (fromJust, fromMaybe)
import           Data.Tuple                         (swap)

data IcmpType =
    IcmpEchoReply |
    IcmpDestUnreach |
    IcmpSourceQuench |
    IcmpRedirect |
    IcmpEcho |
    IcmpRouterAdver |
    IcmpRouterSolic |
    IcmpTimeExceeded |
    IcmpParameterprob |
    IcmpTimestamp |
    IcmpTimestampReply |
    IcmpInfoRequest |
    IcmpInfoReply |
    IcmpAddress |
    IcmpAddressReply |
    IcmpUndefined
    deriving (Bounded, Eq, Ord, Read)

table :: [(IcmpType, Int)]
table = [
    (IcmpEchoReply, 0),
    (IcmpDestUnreach, 3),
    (IcmpSourceQuench, 4),
    (IcmpRedirect, 5),
    (IcmpEcho, 8),
    (IcmpRouterAdver, 9),
    (IcmpRouterSolic, 10),
    (IcmpTimeExceeded, 11),
    (IcmpParameterprob, 12),
    (IcmpTimestamp, 13),
    (IcmpTimestampReply, 14),
    (IcmpInfoRequest, 15),
    (IcmpInfoReply, 16),
    (IcmpAddress, 17),
    (IcmpAddressReply, 18),
    (IcmpUndefined, 196)]

instance Show IcmpType where
    show = (++) "0x" . int2Hex . fromEnum

instance Enum IcmpType where
    succ IcmpEchoReply = IcmpDestUnreach
    succ IcmpRedirect = IcmpEcho
    succ IcmpEcho = IcmpTimeExceeded
    succ x | x == maxBound = succError "IcmpType" | otherwise = toEnum $ fromEnum x + 1

    pred IcmpTimeExceeded = IcmpEcho
    pred IcmpEcho = IcmpRedirect
    pred IcmpDestUnreach = IcmpEchoReply
    pred x | x == minBound = predError "IcmpType" | otherwise = toEnum $ fromEnum x - 1

    fromEnum = fromJust . flip lookup table
    toEnum i = fromMaybe IcmpUndefined $ lookup i (map swap table)

instance Parameter IcmpType
