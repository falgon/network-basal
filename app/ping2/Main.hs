{-# OPTIONS_GHC -Wall -Werror #-}
module Main where

import Prelude hiding (seq)
import ExeUtils.Ping

import Network.Basal.Tools.Icmp.Link
import qualified Network.Basal.Protocols.Utils as PU
import qualified Network.Basal.Protocols.IP.Icmp.Internal as ICMP

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (join, replicateM_, forever)
import Data.IORef
import Data.Tuple.Extra (first, second)
import qualified Network.Info as NI 
import System.Exit (exitFailure)
import System.Posix.Process (getProcessID)
import System.Timeout (timeout)

makePings :: (ICMPResult -> IO ()) -> IORef (Int, Int) -> (Int, Int, Int, String) -> EchoParam -> IO ()
makePings f ref (c, t, i, h) param = do
    putPingHeader h
    ct <- count
    let g = (>>) (incr first ref) . (\x -> echo param { seq = fromIntegral x })
        g' = if t /= 0 then fmap join . timeout (toSec t) . g else g
        f' = ct >>= g' >>= maybe (return ()) ((>>) (incr second ref) . f)
    if c == 0 then forever (f' *> threadDelay (toSec i)) else f' *> replicateM_ (pred c) (threadDelay (toSec i) *> f')

main :: IO ()
main = bracket (PU.setIpForward False) PU.setIpForward $ \_ -> (>>=) parseArgs $ maybe exitFailure $ \(c, t, i, h) -> do
        ref <- newIORef (0, 0)
        n <- PU.getDefaultNIface 
        EchoParam <$> PU.host2addr h 
            <*> return n 
            <*> ICMP.socket (NI.name n) (fromIntegral ICMP.recvMax) 
            <*> getProcessID 
            <*> return 1 >>= makePings disping ref (c, t, i, h) >> readIORef ref >>= pingResult
