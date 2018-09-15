{-# OPTIONS_GHC -Wall #-}
module Network.Basal.Protocols.IP.Identifiers (
    noOptHeaderLength,
    chkTos,
    ServiceType (..),
    Frag (..),
    ProtocolNum (..),
    offMask,
    maxTTL,
    defTTL
) where

import Network.Basal.Protocols.Utils (toEnumError, succError, predError, int2Hex)
import Network.Basal.Protocols.Link.Ether (Parameter)

import Data.Bits ((.&.))
import Data.Word (Word8, Word16)
import Data.Maybe (fromJust, fromMaybe)
import Data.Tuple (swap)

-- | The value is 5 because the data length of the ip header is specified in units of 4 bytes and 
-- when the packet does not have options, its size is 20 bytes.
noOptHeaderLength :: Word8
noOptHeaderLength = 5 

data ServiceType = TosLowDelay | TosThroughPut | TosReliability | TosLowcost deriving (Eq, Ord, Show, Read)

stTable :: [(ServiceType, Int)]
stTable = [
    (TosLowDelay, 0x10),
    (TosThroughPut, 0x08),
    (TosReliability, 0x04),
    (TosLowcost, 0x02)]

chkTos :: ServiceType -> Bool
chkTos = toEnum . (.&. 0x1e) . fromJust . flip lookup stTable

data Frag = Reserved | NotFragment | MoreFragment deriving (Eq, Ord, Bounded, Show, Read)

fragTable :: [(Frag, Int)]
fragTable = [
    (Reserved, 0x8000),
    (NotFragment, 0x4000),
    (MoreFragment, 0x2000)]

instance Enum Frag where
    succ Reserved = NotFragment
    succ NotFragment = MoreFragment
    succ MoreFragment = succError "Frag"
    pred Reserved = predError "Frag"
    pred NotFragment = Reserved
    pred MoreFragment = NotFragment
    fromEnum = fromJust . flip lookup fragTable
    toEnum i = fromMaybe (toEnumError "Frag" i (minBound :: Frag, maxBound :: Frag)) $ lookup i (map swap fragTable)

instance Parameter Frag

offMask :: Word16
offMask = 0x1fff

maxTTL :: Word8
maxTTL = 255

defTTL :: Word8
defTTL = 64

data ProtocolNum = 
    Dummy           |
    Hopopts         |
    Icmp            |
    Igmp            |
    Ipip            |
    Tcp             |
    Egp             |
    Pup             |
    Udp             |
    Idp             |
    Tp              |
    Ipv6            |
    Ipv6Routing     |
    Ipv6Fragment    |
    Rsvp            |
    Gre             |
    Esp             |
    Ah              |
    Icmpv6          |
    None            |
    Dstopts         |
    Mtp             |
    Encap           |
    Pim             |
    Comp            |
    Sctp            |
    Raw
    deriving (Eq, Ord, Bounded, Read)

protocolTable :: [(ProtocolNum, Int)]
protocolTable = [
    (Dummy, 0),
    (Hopopts, 0),
    (Icmp, 1),
    (Igmp, 2),
    (Ipip, 4),
    (Tcp, 6),
    (Egp, 8),
    (Pup, 12),
    (Udp, 17),
    (Idp, 22),
    (Tp, 29),
    (Ipv6, 41),
    (Ipv6Routing, 43),
    (Ipv6Fragment, 44),
    (Rsvp, 46),
    (Gre, 47),
    (Esp, 50),
    (Ah, 51),
    (Icmpv6, 58),
    (None, 59),
    (Dstopts, 60),
    (Mtp, 92),
    (Encap, 98),
    (Pim, 103),
    (Comp, 108),
    (Sctp, 132),
    (Raw, 255)]

instance Show ProtocolNum where
    show = (++) "0x" . int2Hex . fromEnum

instance Enum ProtocolNum where
    fromEnum = fromJust . flip lookup protocolTable
    toEnum i = fromMaybe (toEnumError "Frag" i (minBound :: Frag, maxBound :: Frag)) $ lookup i (map swap protocolTable)

instance Parameter ProtocolNum where
