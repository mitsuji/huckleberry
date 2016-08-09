{-# LANGUAGE ForeignFunctionInterface #-}

module System.PIO.Linux.SPI (
  mode0,
  mode1,
  mode2,
  mode3,
  transferMessage1,
  setRdMode,
  setWrMode,
  setRdLbsFirst,
  setWrLbsFirst,
  setRdBitsPerWord,
  setWrBitsPerWord,
  setRdMaxSpeedHz,
  setWrMaxSpeedHz,
  ) where

import Foreign.Ptr(Ptr)
import Data.Word(Word8,Word32)

foreign import ccall "spi_mode_0" mode0 :: Word8
foreign import ccall "spi_mode_1" mode1 :: Word8
foreign import ccall "spi_mode_2" mode2 :: Word8
foreign import ccall "spi_mode_3" mode3 :: Word8

foreign import ccall "spi_transfer_message_1" transferMessage1 :: Int -> Ptr a -> Int -> Word32 -> Word8 -> IO Int

foreign import ccall "spi_set_rd_mode" setRdMode :: Int -> Word8 -> IO Int
foreign import ccall "spi_set_wr_mode" setWrMode :: Int -> Word8 -> IO Int

foreign import ccall "spi_set_rd_lbs_first" setRdLbsFirst :: Int -> Word8 -> IO Int
foreign import ccall "spi_set_wr_lbs_first" setWrLbsFirst :: Int -> Word8 -> IO Int

foreign import ccall "spi_set_rd_bits_per_word" setRdBitsPerWord :: Int -> Word8 -> IO Int
foreign import ccall "spi_set_wr_bits_per_word" setWrBitsPerWord :: Int -> Word8 -> IO Int

foreign import ccall "spi_set_rd_max_speed_hz" setRdMaxSpeedHz :: Int -> Word32 -> IO Int
foreign import ccall "spi_set_wr_max_speed_hz" setWrMaxSpeedHz :: Int -> Word32 -> IO Int

