{-# OPTIONS_GHC -Wall -Werror #-}
module ExeUtils.Ping (
    count,
    whenArgsUsage,
    usage,
    version,
    parseArgs,
    incr,
    disping,
    pingResult,
    toSec,
    putPingHeader
) where

import Network.Basal.Tools.Icmp.Link
import qualified Network.Basal.Protocols.IP as IP
import qualified Network.Basal.Protocols.IP.Icmp as ICMP

import Data.Char (isDigit)
import Data.List (elemIndex)
import Data.IORef
import Data.Maybe (maybe)
import System.Environment (getArgs, getProgName)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

count :: IO (IO Int)
count = do
    c <- newIORef 0
    return $ readIORef c >>= writeIORef c . succ >> readIORef c

whenArgsUsage :: [String] -> Bool
whenArgsUsage [] = True
whenArgsUsage ["-h"] = True
whenArgsUsage ["--help"] = True
whenArgsUsage _ = False

usage :: IO ()
usage = getProgName >>= putStrLn . (++) "usage: " . flip (++) " [-c count] [-t timeout] [-i wait] host"

version :: IO ()
version = getProgName >>= putStrLn . flip (++) " version 1.0.0"

findNextDigitable :: String -> [String] -> Maybe ([String], [String])
findNextDigitable x ls = maybe Nothing (\i -> if succ i < length ls && isDigit' (ls !! succ i) then Just (take 2 (drop i ls), take i ls ++ drop (i + 2) ls) else Nothing) $ elemIndex x ls
    where
        isDigit' = null . dropWhile isDigit

parseArgs :: IO (Maybe (Int, Int, Int, String))
parseArgs = do
    args <- getArgs
    case args of
        ["--version"] -> Nothing <$ version
        _ | whenArgsUsage args -> usage >> return Nothing | otherwise -> do
            let c = findNextDigitable "-c" args
                otc = maybe args snd c
                t = findNextDigitable "-t" otc
                ott = maybe otc snd t
                i = findNextDigitable "-i" ott
                oti = maybe ott snd i
                h = maybe oti snd i
            if length h == 1 then return $ Just (maybe 0 (read . last . fst) c :: Int, maybe 0 (read . last . fst) t :: Int, maybe 1 (read . last. fst) i :: Int, head h) else Nothing <$ usage 

incr :: Enum a1 => ((a1 -> a1) -> a2 -> a2) -> IORef a2 -> IO ()
incr f ref = writeIORef ref . f succ =<< readIORef ref
            
disping :: ICMPResult -> IO ()
disping x = putStrLn $ show (dataLength x) ++ " bytes" ++ maybe ": " (flip (++) ": " . (++) " from ") (sender x) ++
    "icmp_seq=" ++ show (ICMP.icmpSeq $ icmpH x) ++ " ttl=" ++ show (IP.ipTtl $ ipH x) ++ " time=" ++ show (delay x)
        
        
pingResult :: (Int, Int) -> IO ()
pingResult (s, recv) = if s /= 0 then putStrLn "\n--- ping statics ---" *>
    putStr (show s ++ " packets transmitted, " ++ show recv ++ " received, ") *>
    printf "%.0f" (100 - (fromIntegral recv / fromIntegral s :: Float) * 100) *> 
    putStrLn "% packet loss" else hPutStrLn stderr "Failed to send packets."  *> exitFailure

toSec :: Integral a => a -> a
toSec = (*1000) . (*1000)
    
putPingHeader :: String -> IO ()
putPingHeader h = putStrLn $ "PING " ++ h ++ ": " ++ show dataSize ++ " data bytes"
