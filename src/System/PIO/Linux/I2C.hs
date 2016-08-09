{-# LANGUAGE ForeignFunctionInterface #-}

module System.PIO.Linux.I2C (
  setSlave
  ) where

import Data.Word(Word8)

foreign import ccall "i2c_set_slave" setSlave :: Int -> Word8 -> IO Int

