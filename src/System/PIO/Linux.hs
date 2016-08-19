{-# LANGUAGE ForeignFunctionInterface #-}

module System.PIO.Linux (
  fdOpen,
  fdClose,
  fdPutBuf,
  fdGetBuf,
  oRdOnly,
  oWrOnly,
  oRdWr
  ) where

import Foreign.Ptr(Ptr)
import Foreign.C.String(CString,withCString)
  
foreign import ccall "open" open :: CString -> Int -> IO Int
foreign import ccall "close" fdClose :: Int -> IO Int
                                        
foreign import ccall "write" fdPutBuf :: Int -> Ptr a -> Int -> IO Int
foreign import ccall "read" fdGetBuf :: Int -> Ptr a -> Int -> IO Int
                                        
foreign import ccall "o_rdonly" oRdOnly :: Int
foreign import ccall "o_wronly" oWrOnly :: Int
foreign import ccall "o_rdwr" oRdWr :: Int
                                        
fdOpen :: String -> Int -> IO Int
fdOpen path flags = withCString path $ \cs -> open cs flags
                                              

