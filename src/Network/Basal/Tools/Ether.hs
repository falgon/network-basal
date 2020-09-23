module Network.Basal.Tools.Ether (
    getDstMacAddr
) where

import qualified Network.Basal.Protocols.IP.Internal as IP
import           Network.Basal.Subnet
import qualified Network.Basal.Tools.Arp             as ARP

import qualified Network.Info                        as NI
import qualified Network.Socket                      as NS

-- | Let X be the specified ip address.
-- If X is included in the subnet to which the specified nic belongs,
-- it will obtain the MAC address of X (possibly by issuing an ARP request),
-- otherwise it will return the MAC address of the default route gateway.
getDstMacAddr :: NI.NetworkInterface -> NS.SockAddr -> IO (Maybe NI.MAC)
getDstMacAddr src (NS.SockAddrInet p dst) = (>>=) (getSubnetMaskFromNIface $ NI.name src) $ maybe (return Nothing) $ \subm -> if isin srcip dst subm then ARP.getMacAddr src (NS.SockAddrInet p dst) (5*1000*1000) else IP.getDGWMacAddr src (5*1000*1000)
    where
        (NI.IPv4 srcip) = NI.ipv4 src
getDstMacAddr _ _ = error "sorry, implemented only ipv4 yet."
