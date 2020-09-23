module Main where

import           Data.Int                (Int64)
import qualified Network.Basal.Tools.Arp as ARP
import           System.Environment      (getArgs, getProgName)
import           System.Exit             (exitFailure)
import           System.IO               (hPutStrLn, stderr)

timeOuts :: Int64
timeOuts = 5 * 1000 * 1000

main :: IO ()
main = do
    args <- getArgs
    if length args /= 2 then getProgName >>= hPutStrLn stderr . (++) "usage: " . flip (++) " <network interface name> <ip address>" >> exitFailure else
        ARP.getMacAddr' (head args) (last args) timeOuts >>= print
