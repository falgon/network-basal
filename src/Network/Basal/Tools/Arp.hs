{-# OPTIONS_GHC -Wall #-}
module Network.Basal.Tools.Arp (
    getMacAddr,
    getMacAddr'
) where

import           Network.Basal.Protocols.Link.Arp.Internal (arpSenderMac,
                                                            ethPacket,
                                                            ethPacket',
                                                            lookupArpTable,
                                                            socket, socket')
import           Network.Basal.Protocols.Link.Ether        (ethData, recvFrom,
                                                            send)
import           Network.Basal.Protocols.Utils             (host2ipv4, l2m)

import           Data.Int                                  (Int64)
import           Data.Maybe                                (maybe)
import           Data.Tuple.Extra                          (dupe, first, second)
import           Network.Info                              (IPv4 (..), MAC,
                                                            NetworkInterface)
import           Network.Socket                            (SockAddr (..))

-- | If a mapping exists in the arp table, it refers to it and returns it.
-- Otherwise it issues an arp request and returns the result.
getMacAddr :: NetworkInterface -> SockAddr -> Int64 -> IO (Maybe MAC)
getMacAddr nic (SockAddrInet p host) timeout = (>>=) (lookupArpTable $ IPv4 host) $ flip maybe (return . Just) $ do
    sock <- socket nic timeout
    send sock $ ethPacket nic (SockAddrInet p host)
    l2m . arpSenderMac . ethData . snd <$> recvFrom sock
getMacAddr _ _ _ = error "sorry, only implemented ipv4 yet"

-- | It is similar to `getMacAddr`. It takes a string as an argument.
getMacAddr' :: String -> String -> Int64 -> IO (Maybe MAC)
getMacAddr' nic host timeout = (>>=) ((>>=) (host2ipv4 host) lookupArpTable) $ flip maybe (return . Just) $ (>>=) (ethPacket' nic host) $ maybe (return Nothing) $ \p ->
    (socket' nic timeout >>= (uncurry id . first ((>>) . flip send p) . second return . dupe)) >>= fmap (l2m . arpSenderMac . ethData . snd) . recvFrom
