{-|
Module      : Network.Basal.Protocols.Utils
Description : Some small utilities
Copyright   : (C) Roki, 2018
License     : MIT
Maintainer  : falgon53@yahoo.co.jp
Stability   : experimental
Portability : POSIX
-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE CPP #-}
module Network.Basal.Protocols.Utils (
    getDefaultNIface,
    findNIface,
    host2addr,
    host2ipv4,
    int2Hex,
    hex2Int,
    succError,
    predError,
    toEnumError,
    setIpForward,
    m2l,
    l2m
) where

import Data.Char (digitToInt, intToDigit)
import Data.Word (Word8)
import Data.List (foldl', find, unfoldr)
import Data.Tuple.Extra (first, second, dupe)
import Data.Maybe (fromMaybe)
import qualified Network.Info as NI
import qualified Network.Socket as NS
import System.IO (IOMode (ReadWriteMode), hGetChar, hPutChar, withFile)
#ifdef __GLASGOW_HASKELL__ 
import GHC.Enum (succError, predError, toEnumError)
#else
{-# NOINLINE succError #-}
succError :: String -> a
succError inst_ty =
    errorWithoutStackTrace $ "Enum.succ{" ++ inst_ty ++ "}: tried to take `succ' of maxBound"

{-# NOINLINE predError #-}
predError :: String -> a
predError inst_ty =
    errorWithoutStackTrace $ "Enum.pred{" ++ inst_ty ++ "}: tried to take `pred' of minBound"

{-# NOINLINE toEnumError #-}
toEnumError :: (Show a) => String -> Int -> (a,a) -> b
toEnumError inst_ty i bnds = errorWithoutStackTrace $ "Enum.toEnum{" ++ inst_ty ++ "}: tag (" ++ show i ++ ") is outside of bounds " ++ show bnds
#endif

getDefaultNIface :: IO NI.NetworkInterface
getDefaultNIface = fromMaybe (error "Could not found available network interfaces.") . find (uncurry (&&) . first ((/=NI.IPv4 0) . NI.ipv4) . second ((/="lo") . NI.name) . dupe) <$> NI.getNetworkInterfaces

findNIface :: String -> IO (Maybe NI.NetworkInterface)
findNIface expectedNIName = find ((expectedNIName==) . NI.name) <$> NI.getNetworkInterfaces

host2addr :: String -> IO NS.SockAddr
host2addr = fmap (NS.addrAddress . head) . flip (NS.getAddrInfo Nothing) Nothing . Just

host2ipv4 :: String -> IO NI.IPv4
host2ipv4 host = do
    addr <- host2addr host
    let (NS.SockAddrInet _ ip) = addr
    return $ NI.IPv4 ip

int2Hex :: Integral a => a -> String
int2Hex = map (intToDigit . fromIntegral) . reverse . unfoldr (\x -> if x == 0 then Nothing else Just (mod x 16, div x 16))

hex2Int :: String -> Int
hex2Int = foldl' (flip (.) digitToInt . (+) . (16*)) 0 

setIpForward :: Bool -> IO Bool
setIpForward x = withFile "/proc/sys/net/ipv4/ip_forward" ReadWriteMode $ \h -> do
    b <- digitToInt <$> hGetChar h
    hPutChar h $ intToDigit $ fromEnum x
    return $ toEnum b

-- | NI.MAC to [Word8]
m2l :: NI.MAC -> [Word8]
m2l (NI.MAC b1 b2 b3 b4 b5 b6) = [b1, b2, b3, b4, b5, b6]

-- | [Word8] to Maybe NI.Mac
l2m :: [Word8] -> Maybe NI.MAC
l2m [b1, b2, b3, b4, b5, b6] = Just $ NI.MAC b1 b2 b3 b4 b5 b6
l2m _ = Nothing
