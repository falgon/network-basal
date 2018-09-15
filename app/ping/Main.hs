{-# OPTIONS_GHC -Wall -Werror #-}
module Main where

import Prelude hiding (seq)
import ExeUtils.Ping

import Network.Basal.Tools.Icmp.Inet (echo, ICMPResult)
import qualified Network.Basal.Protocols.IP.Icmp as NI (socketINET)
import Network.Basal.Protocols.Utils (setIpForward)

import Control.Concurrent (threadDelay)
import Control.Exception (bracket)
import Control.Monad (forever, replicateM_, join)
import Data.IORef
import Data.Tuple.Extra (first, second)
import Data.Maybe (maybe)
import Network.Socket (close, Socket)
import System.Exit (exitFailure)
import System.Timeout (timeout)
import System.Posix.Process (getProcessID)

makePings :: (ICMPResult -> IO ()) -> IORef (Int, Int) -> (Int, Int, Int, String) -> Socket -> IO ()
makePings f ref (c, t, i, h) sock = do
    putPingHeader h
    pid <- getProcessID
    ct <- count
    let g = (>>) (incr first ref) . echo h sock pid . fromIntegral
        g' = if t /= 0 then fmap join . timeout (toSec t) . g else g
        f' = ct >>= g' >>= maybe (return ()) ((>>) (incr second ref) . f)
    if c == 0 then forever (f' *> threadDelay (toSec i)) else f' *> replicateM_ (pred c) (threadDelay (toSec i) *> f')

main :: IO ()
main = bracket (setIpForward False) setIpForward $ \_ -> do
    ref <- newIORef (0, 0) 
    (>>=) parseArgs $ maybe exitFailure $ bracket NI.socketINET close . makePings disping ref 
    readIORef ref >>= pingResult
