{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
module Network.Basal.Subnet (
    getSubnetMaskFromNIface,
    isin
) where

import qualified Network.Socket as NS
import Control.Exception (bracket)
import Data.Bool (bool)
import Data.Bits (shiftL, (.&.))
import Data.Word (Word32)
import qualified Network.Socket as NS hiding (recv, recvFrom, send, sendTo)

import Foreign.C.String (CString, newCString)
import Foreign.C.Types (CUInt (..), CInt (..))
import Foreign.Marshal.Alloc (allocaBytes, free)
import Foreign.Storable (peek)
import Foreign.Ptr (Ptr)

#include "get_network_subnet.h"

foreign import ccall unsafe "get_subnet_from_nic"
    c_get_subnet_from_nic :: CString -> Ptr CUInt -> IO CInt

#{enum CInt, CInt
, exitsucc = EXIT_SUCCESS
, exitfail = EXIT_FAILURE
}

getSubnetMaskFromNIface :: String -> IO (Maybe NS.HostAddress)
getSubnetMaskFromNIface nic = allocaBytes 4 $ \ptr -> bracket (newCString nic) free $ \cs -> c_get_subnet_from_nic cs ptr >>= bool (return Nothing) (Just . fromIntegral <$> peek ptr) . (==exitsucc)

isin :: NS.HostAddress -> NS.HostAddress -> NS.HostAddress -> Bool
isin ip1 ip2 smask = (ip1 .&. smask) == (ip2 .&. smask)
