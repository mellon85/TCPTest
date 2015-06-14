{-# LANGUAGE ForeignFunctionInterface #-}

module TCPTest.FFIInterface 
    (TCPInfo(..),
     printTCPInfoHeader,
     socketInfo)
    where

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.Word
import Text.Printf

-- all 32 bits to avoid alignment issues
data TCPInfo = Info {
        state                   :: Word32 , -- 8 bit
        retransmitted           :: Word32 , -- 8 bit
        lostPackets             :: Word32 ,
        rtt                     :: Word32 ,
        rttVar                  :: Word32 ,
        backoff                 :: Word32 } -- 8 bit

printTCPInfoHeader :: IO ()
printTCPInfoHeader =
    printf "%5s %7s %9s %9s %9s %7s\n"
        "state"      "retrans"    "lost"
        "rtt"        "rttVar"     "backoff"

instance Show TCPInfo where
    show e = printf "%5d %7d %9d %9d %9d %7d"
        (state e) (retransmitted e) (lostPackets e) (rtt e) (rttVar e) (backoff e)

instance Storable TCPInfo where
    sizeOf _ = 6 * (sizeOf (0::Word32))
    alignment = sizeOf
    poke p v = pokeByteOff p 0 v
    peek ptr = do
        a <- peekByteOff ptr 0
        b <- peekByteOff ptr 4
        c <- peekByteOff ptr 8
        d <- peekByteOff ptr 12
        e <- peekByteOff ptr 16
        f <- peekByteOff ptr 20
        return $! Info a b c d e f

foreign import ccall safe "getsocketinfo" getsocketinfo_ffi ::  CInt -> Ptr TCPInfo -> IO CInt 

socketInfo :: CInt -> IO (Either Int TCPInfo)
socketInfo fd = do
    ptr  <- malloc 
    ret  <- getsocketinfo_ffi fd ptr
    info <- case ret of
                0  -> peek ptr >>= (return . Right)
                _  -> return $! Left $ fromIntegral ret
    free ptr
    return info

{-

from /usr/include/netinet/tcp.h (state)
enum
{
  TCP_ESTABLISHED = 1,
  TCP_SYN_SENT,
  TCP_SYN_RECV,
  TCP_FIN_WAIT1,
  TCP_FIN_WAIT2,
  TCP_TIME_WAIT,
  TCP_CLOSE,
  TCP_CLOSE_WAIT,
  TCP_LAST_ACK,
  TCP_LISTEN,
  TCP_CLOSING   /* now a valid state */
};

-}
