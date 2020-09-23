{-# OPTIONS_GHC -Wall #-}
module Test.Protocols.Link.Arp.LookupTable (
    randomMac,
    randomIp,
    randomRow,
    createPseudoArpTable,
    test
) where

import           Network.Basal.Protocols.Link.Arp.Internal (lookupATP)
import           Network.Basal.Protocols.Utils             (hex2Int, host2ipv4,
                                                            int2Hex, l2m)
import qualified Test.Protocols.Link.TestUtils             as TT

import           Control.Exception                         (bracket)
import           Control.Monad                             (replicateM,
                                                            replicateM_)
import           Data.Bool                                 (bool)
import           Data.List.Split                           (wordsBy)
import           Data.Tuple.Extra                          (dupe, first, second)
import           System.Directory                          (removeFile)
import           System.IO                                 (IOMode (..),
                                                            hPutStr, withFile)
import           System.Random                             (randomRIO)
import           Test.HUnit                                ((@?=))

testmodule :: String -> TT.Assertion -> TT.Test
testmodule = TT.testmodule . (++) "ARP."

randIS :: Int -> Int -> IO String
randIS mi ma = (show :: Int -> String) <$> (randomRIO (mi, ma) :: IO Int)

addDelim :: String -> Int -> [String] -> String
addDelim dot n = concat . zipWith (\i x -> if n /= i then x ++ dot else x) [1..n]

randomMac :: IO String
randomMac = addDelim ":" 6 <$> replicateM 6 (uncurry id . first (\x -> bool x ('0':x)) . second ((==1) . length) . dupe . int2Hex <$> (randomRIO (0, 255) :: IO Int))

randomIp :: IO String
randomIp = (randomRIO (0, 2) :: IO Int) >>= rndip
    where
        rndip 0 = (++) "10." . addDelim "." 3 <$> replicateM 3 (randIS 0 255) -- class A
        rndip 1 = (++) "172." . addDelim "." 3 <$> ((:) <$> randIS 16 31 <*> replicateM 2 (randIS 0 255)) -- class B
        rndip 2 = (++) "192.168." . addDelim "." 2 <$> replicateM 2 (randIS 0 255) -- class C
        rndip _ = error "unreach"

randomRow :: IO String
randomRow = do
    ri <- randomIp
    rm <- randomMac
    return $ ri ++ "\t0x1\t0x2\t" ++ rm ++ "\t*\teth0\n"

createPseudoArpTable :: String -> Int -> String -> String -> IO String
createPseudoArpTable fname dummyRowLen hitip hitmac = withFile fname WriteMode $ \h -> do
    hPutStr h "Address\tHW type\tFlags\tHW address\tMask\tDevice\n"
    hitline <- randomRIO (0, dummyRowLen) :: IO Int
    replicateM_ hitline (randomRow >>= hPutStr h)
    hPutStr h $ hitip ++ "\t0x1\t0x2\t" ++ hitmac ++ "\t*\teth0\n"
    replicateM_ (dummyRowLen - hitline) (randomRow >>= hPutStr h)
    return fname

test :: IO TT.Test
test = do
    let hitm = "11:22:33:44:55:66"
        hiti = "10.0.2.3"
    ip <- host2ipv4 hiti
    lkp <- bracket (createPseudoArpTable "./arp" 6 hiti hitm) removeFile $ lookupATP ip
    return $ testmodule "LookupTable.test: " $ l2m (map (fromIntegral . hex2Int) $ wordsBy (==':') hitm) @?= lkp
