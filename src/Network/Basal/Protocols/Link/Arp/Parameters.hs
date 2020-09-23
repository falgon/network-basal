{-|
Module      : Network.Basal.Protocols.Link.Arp.Parameters
Description : The parameters of arp protocol.
Copyright   : (C) Roki, 2018
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
{-# OPTIONS_GHC -Wall #-}

module Network.Basal.Protocols.Link.Arp.Parameters (
    OperationCode (..),
    HardwareType (..),
) where

import qualified Network.Basal.Protocols.Link.Ether as LE
import           Network.Basal.Protocols.Utils      (int2Hex, predError,
                                                     succError, toEnumError)

import           Data.Maybe                         (fromJust, fromMaybe)
import           Data.Tuple                         (swap)

data OperationCode =
    OpcResv0        | -- RFC 5494
    OpcReq          | -- RFC 826, 5227
    OpcRep          | -- RFC 826, 5227
    OpcReqRev       | -- RFC 903
    OpcRepRev       | -- RFC 903
    OpcDRReq        | -- RFC 1931
    OpcDRRep        | -- RFC 1931
    OpcDRErr        | -- RFC 1931
    OpcInReq        | -- RFC 2390
    OpcInRep        | -- RFC 2390
    OpcNak          | -- RFC 1577
    OpcMARSReq      |
    OpcMARSMul      |
    OpcMARSMServ    |
    OpcMARSJoin     |
    OpcMARSLeave    |
    OpcMARSNak      |
    OpcMARSUnserv   |
    OpcMARSSJoin    |
    OpcMARSSLeave   |
    OpcMARSGLReq    |
    OpcMARSGLRep    |
    OpcMARSRMap     |
    OpcMARPOSUn     | -- RFC 2176
    OpcOPExp1       |
    OpcOPExp2       |
    OpcResv1          -- RFC5494
    deriving (Bounded, Enum, Eq, Ord, Read)

instance Show OperationCode where
    show = (++) "0x" . int2Hex . fromEnum

instance LE.Parameter OperationCode

data HardwareType =
    HrdtResv0           | -- RFC 5494
    HrdtEthernet        |
    HrdtEEthernet       |
    HrdtAX25            |
    HrdtProNETRing      |
    HrdtChaos           |
    HrdtIEEE802         |
    HrdtArcNet          | -- RFC 1201
    HrdtHyperchan       |
    HrdtLanstar         |
    HrdtAutoShortAddr   |
    HrdtLocalTalk       |
    HrdtLocalNet        |
    HrdtUltraLink       |
    HrdtSMDS            |
    HrdtFrameRelay      |
    HrdtATM16           |
    HrdtHDLC            |
    HrdtFibreChan       | -- RJC 4338
    HrdtATM19           | -- RFC 2225
    HrdtSerialLine      |
    HrdtATM21           |
    HrdtMILSTD          |
    HrdtMetricom        |
    HrdtIEEE1394        |
    HrdtMAPOS           |
    HrdtTwinaxial       |
    HrdtEUI64           |
    HrdtHIPARP          |
    HrdtIAI7816         |
    HrdtARPSec          |
    HrdtIPtun           | -- RFC 3456
    HrdtInfiniBand      | -- RFC 4391
    HrdtCAI             |
    HrdtWI              |
    HrdtPureIP          |
    HrdtHWExp1          | -- RFC 5494
    HrdtHFI             |
    HrdtHwExp2          | -- RFC 5494
    HrdtAEthernet       |
    HrdtResv1             -- RFC 5494
    deriving (Bounded, Eq, Ord, Read)

instance Show HardwareType where
    show = (++) "0x" . int2Hex . fromEnum

table :: [(HardwareType, Int)]
table = [
    (HrdtResv0, 0),
    (HrdtEthernet, 1),
    (HrdtEEthernet, 2),
    (HrdtAX25, 3),
    (HrdtProNETRing, 4),
    (HrdtChaos, 5),
    (HrdtIEEE802, 6),
    (HrdtArcNet, 7),
    (HrdtHyperchan, 8),
    (HrdtLanstar, 9),
    (HrdtAutoShortAddr, 10),
    (HrdtLocalTalk, 11),
    (HrdtLocalNet, 12),
    (HrdtUltraLink, 13),
    (HrdtSMDS, 14),
    (HrdtFrameRelay, 15),
    (HrdtATM16, 16),
    (HrdtHDLC, 17),
    (HrdtFibreChan, 18),
    (HrdtATM19, 19),
    (HrdtSerialLine, 20),
    (HrdtATM21, 21),
    (HrdtMILSTD, 22),
    (HrdtMetricom, 23),
    (HrdtIEEE1394, 24),
    (HrdtMAPOS, 25),
    (HrdtTwinaxial, 26),
    (HrdtEUI64, 27),
    (HrdtHIPARP, 28),
    (HrdtIAI7816, 29),
    (HrdtARPSec, 30),
    (HrdtIPtun, 31),
    (HrdtInfiniBand, 32),
    (HrdtCAI, 33),
    (HrdtWI, 34),
    (HrdtPureIP, 35),
    (HrdtHWExp1, 36),
    (HrdtHFI, 37),
    (HrdtHwExp2, 256),
    (HrdtAEthernet, 257),
    (HrdtResv1, 65535)]

instance Enum HardwareType where
    succ HrdtHFI = HrdtHwExp2
    succ HrdtHwExp2 = HrdtAEthernet
    succ HrdtAEthernet = HrdtResv1
    succ x | x /= maxBound = toEnum $ fromEnum x + 1 | otherwise = succError "HardwareType"

    pred HrdtResv1 = HrdtAEthernet
    pred HrdtAEthernet = HrdtHwExp2
    pred HrdtHwExp2 = HrdtHFI
    pred x | x /= minBound = toEnum $ fromEnum x - 1 | otherwise = predError "HardwareType"

    fromEnum = fromJust . flip lookup table
    toEnum i = fromMaybe (toEnumError "HardwareType" i (minBound :: HardwareType, maxBound :: HardwareType)) $ lookup i (map swap table)

instance LE.Parameter HardwareType
