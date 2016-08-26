{-# LANGUAGE ForeignFunctionInterface #-}

module System.PIO.Linux.I2C.Raw (
  SlaveAddress,
  setSlave
  ) where

import Data.Word(Word8)
import System.PIO.Linux(FileDescriptor)
import Foreign.C.Error(throwErrno)

type SlaveAddress = Word8

foreign import ccall "i2c_set_slave" i2c_set_slave :: Int -> Word8 -> IO Int


-- | Computation 'setSlave' @fd@ @address@ sets the slave address for
-- descriptor @fd@ on I2C.
setSlave :: FileDescriptor -> SlaveAddress -> IO ()
setSlave fd addr = i2c_set_slave fd addr >>= \rc -> case rc of
  -1 -> throwErrno "setSlave"
  otherwize -> return ()

