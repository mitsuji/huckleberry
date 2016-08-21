{-# LANGUAGE ForeignFunctionInterface #-}

module System.PIO.Linux.SPI.Raw (
  SPIMode(..),
  Bits,
  Speed,
  transferTxRx1,
  transferTxRx2,
  transferTx,
  transferRx,
  getMode,
  setMode,
  getLsbFirst,
  setLsbFirst,
  getBitsPerWord,
  setBitsPerWord,
  getMaxSpeedHz,
  setMaxSpeedHz,
  ) where

import Foreign.Ptr(Ptr)
import Data.Word(Word8,Word16,Word32)
import System.PIO.Linux(FileDescriptor)
import Foreign.Marshal.Alloc(alloca)
import Foreign.Storable(peek)
import Foreign.C.Error(throwErrno)


type Bits  = Word8
type Speed = Word32
type Delay = Word16
type CSChange = Bool


foreign import ccall "spi_mode_0" spi_mode0 :: Word8
foreign import ccall "spi_mode_1" spi_mode1 :: Word8
foreign import ccall "spi_mode_2" spi_mode2 :: Word8
foreign import ccall "spi_mode_3" spi_mode3 :: Word8

foreign import ccall "spi_transfer_tx_rx_1" spi_transfer_tx_rx_1
  :: Int -> Ptr a -> Int -> Word8 -> Word32 -> Word16 -> Word8 -> IO Int
foreign import ccall "spi_transfer_tx_rx_2" spi_transfer_tx_rx_2
  :: Int -> Ptr a -> Int -> Ptr a -> Int -> Word8 -> Word32 -> Word16 -> Word8 -> IO Int
foreign import ccall "spi_transfer_tx_1" spi_transfer_tx_1
  :: Int -> Ptr a -> Int -> Word8 -> Word32 -> Word16 -> Word8 -> IO Int
foreign import ccall "spi_transfer_rx_1" spi_transfer_rx_1
  :: Int -> Ptr a -> Int -> Word8 -> Word32 -> Word16 -> Word8 -> IO Int

foreign import ccall "spi_get_mode" spi_get_mode :: Int -> Ptr Word8 -> IO Int
foreign import ccall "spi_set_mode" spi_set_mode :: Int -> Word8 -> IO Int

foreign import ccall "spi_get_lsb_first" spi_get_lsb_first :: Int -> Ptr Word8 -> IO Int
foreign import ccall "spi_set_lsb_first" spi_set_lsb_first :: Int -> Word8 -> IO Int

foreign import ccall "spi_get_bits_per_word" spi_get_bits_per_word :: Int -> Ptr Word8 -> IO Int
foreign import ccall "spi_set_bits_per_word" spi_set_bits_per_word :: Int -> Word8 -> IO Int

foreign import ccall "spi_get_max_speed_hz" spi_get_max_speed_hz :: Int -> Ptr Word32 -> IO Int
foreign import ccall "spi_set_max_speed_hz" spi_set_max_speed_hz :: Int -> Word32 -> IO Int



data SPIMode = Mode0
             | Mode1
             | Mode2
             | Mode3
              
                                        
instance Enum SPIMode where
  
  toEnum n | (fromIntegral n) == spi_mode0 = Mode0
           | (fromIntegral n) == spi_mode1 = Mode1
           | (fromIntegral n) == spi_mode2 = Mode2
           | (fromIntegral n) == spi_mode3 = Mode3
           | otherwise  = error "toEnum: convert error"

  fromEnum Mode0 = (fromIntegral spi_mode0)
  fromEnum Mode1 = (fromIntegral spi_mode1)
  fromEnum Mode2 = (fromIntegral spi_mode2)
  fromEnum Mode3 = (fromIntegral spi_mode3)



-- | 'transferTxRx1' @fd@ @buf@ @count@ @bits@ @speed@ @delay@ @csChange@ transfer @count@ 8-bit bytes from the
-- buffer @buf@ to the descriptor @fd@.
transferTxRx1 :: FileDescriptor -> Ptr a -> Int -> Bits -> Speed -> Delay -> CSChange -> IO ()
transferTxRx1 fd buf bufLen bits speed delay csChange =
  spi_transfer_tx_rx_1 fd buf bufLen bits speed delay (fromIntegral $ fromEnum csChange)
  >>= \rc -> case rc of
    -1 -> throwErrno "transferTxRx1"
    otherwize -> return ()
    

transferTxRx2 :: FileDescriptor -> Ptr a -> Int -> Ptr a -> Int -> Bits -> Speed -> Delay -> CSChange -> IO ()
transferTxRx2 fd txBuf txBufLen rxBuf rxBufLen bits speed delay csChange =
  spi_transfer_tx_rx_2 fd txBuf txBufLen rxBuf rxBufLen bits speed delay (fromIntegral $ fromEnum csChange)
  >>= \rc -> case rc of
    -1 -> throwErrno "transferTxRx2"
    otherwize -> return ()
  

transferTx :: FileDescriptor -> Ptr a -> Int -> Bits -> Speed -> Delay -> CSChange -> IO ()
transferTx fd buf bufLen bits speed delay csChange =
  spi_transfer_tx_1 fd buf bufLen bits speed delay (fromIntegral $ fromEnum csChange)
  >>= \rc -> case rc of
    -1 -> throwErrno "transferTx"
    otherwize -> return ()
    

transferRx :: FileDescriptor -> Ptr a -> Int -> Bits -> Speed -> Delay -> CSChange -> IO ()
transferRx fd buf bufLen bits speed delay csChange =
  spi_transfer_rx_1 fd buf bufLen bits speed delay (fromIntegral $ fromEnum csChange)
  >>= \rc -> case rc of
    -1 -> throwErrno "transferRx"
    otherwize -> return ()



-- | Computation 'getMode' @fd@ gets the spi mode for
-- descriptor @fd@.
getMode :: FileDescriptor -> IO SPIMode
getMode fd = alloca $
             \p -> spi_get_mode fd p >>= \rc -> case rc of
               -1 -> throwErrno "getMode"
               otherwize -> do
                 mode <- peek p
                 return $ toEnum $ fromIntegral mode


-- | Computation 'setMode' @fd@ @mode@ sets the spi mode for
-- descriptor @fd@.
setMode :: FileDescriptor -> SPIMode -> IO ()
setMode fd mode =
  spi_set_mode fd (fromIntegral $ fromEnum mode)
  >>= \rc -> case rc of
    -1 -> throwErrno "setMode"
    otherwize -> return ()


-- | Computation 'getLsbFirst' @fd@ gets the LBS-first flag for
-- descriptor @fd@.
getLsbFirst :: FileDescriptor -> IO Bool
getLsbFirst fd = alloca $
                 \p -> spi_get_lsb_first fd p >>= \rc -> case rc of
                   -1 -> throwErrno "getLsbFirst"
                   otherwize -> do
                     flag <- peek p
                     return $ toEnum $ fromIntegral flag


-- | Computation 'setLsbFirst' @fd@ @flag@ sets the LBS-first flag for
-- descriptor @fd@.
setLsbFirst :: FileDescriptor -> Bool -> IO ()
setLsbFirst fd flag =
  spi_set_lsb_first fd (fromIntegral $ fromEnum flag)
  >>= \rc -> case rc of
    -1 -> throwErrno "setLsbFirst"
    otherwize -> return ()


-- | Computation 'getBitsPerWord' @fd@ gets the number
-- of bits in each SPI transfer word for
-- descriptor @fd@.
getBitsPerWord :: FileDescriptor -> IO Bits
getBitsPerWord fd = alloca $
  \p -> spi_get_bits_per_word fd p >>= \rc -> case rc of
    -1 -> throwErrno "getBitsPerWord"
    otherwize -> peek p


-- | Computation 'setBitsPerWord' @fd@ @bits@ sets the number
-- of bits in each SPI transfer word for
-- descriptor @fd@.
setBitsPerWord :: FileDescriptor -> Bits -> IO ()
setBitsPerWord fd bits = spi_set_bits_per_word fd bits
  >>= \rc -> case rc of
    -1 -> throwErrno "setBitsPerWord"
    otherwize -> return ()


-- | Computation 'getMaxSpeedHz' @fd@ gets the maximum
-- SPI transfer speed, in Hz for
-- descriptor @fd@.
getMaxSpeedHz :: FileDescriptor -> IO Speed
getMaxSpeedHz fd = alloca $
  \p -> spi_get_max_speed_hz fd p >>= \rc -> case rc of
    -1 -> throwErrno "getMaxSpeedHz"
    otherwize -> peek p


-- | Computation 'setMaxSpeedHz' @fd@ @speed@ sets the maximum
-- SPI transfer speed, in Hz for
-- descriptor @fd@.
setMaxSpeedHz :: FileDescriptor -> Speed -> IO ()
setMaxSpeedHz fd speed = spi_set_max_speed_hz fd speed
  >>= \rc -> case rc of
    -1 -> throwErrno "setMaxSpeedHz"
    otherwize -> return ()


