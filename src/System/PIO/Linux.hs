{-# LANGUAGE ForeignFunctionInterface #-}

module System.PIO.Linux (
  FilePath,
  FileDescriptor,
  IOMode(..),
  fdOpen,
  fdClose,
  fdPutBuf,
  fdGetBuf,
  ) where

import Foreign.Ptr(Ptr)
import Foreign.C.String(CString,withCString)
import Prelude hiding (FilePath)
import Foreign.C.Error(throwErrno)

-- | File and directory names are values of type String
type FilePath = String

-- | File descriptor numbers are values of type Int
type FileDescriptor = Int
                

foreign import ccall "o_rdonly" o_rdonly :: Int
foreign import ccall "o_wronly" o_wronly :: Int
foreign import ccall "o_rdwr" o_rdwr :: Int
                                        
foreign import ccall "open" linux_open :: CString -> Int -> IO Int
foreign import ccall "close" linux_close :: Int -> IO Int
foreign import ccall "write" linux_write :: Int -> Ptr a -> Int -> IO Int
foreign import ccall "read" linux_read :: Int -> Ptr a -> Int -> IO Int

                
data IOMode = ReadMode
            | WriteMode
            | ReadWriteMode
              
instance Enum IOMode where
  
  toEnum n | n == o_rdonly = ReadMode
           | n == o_wronly = WriteMode
           | n == o_rdwr = ReadWriteMode
           | otherwise = error "toEnum: convert error"

  fromEnum ReadMode = o_rdonly
  fromEnum WriteMode = o_wronly
  fromEnum ReadWriteMode = o_rdwr



-- | Computation 'fdOpen' @file@ @mode@ allocates and returns a new, open
-- descriptor to manage the file @file@. It manages input if @mode@
-- is 'ReadMode', output if @mode@ is 'WriteMode',
-- and both input and output if mode is 'ReadWriteMode'.
fdOpen :: FilePath -> IOMode -> IO FileDescriptor
fdOpen path mode = withCString path $
  \cpath -> linux_open cpath (fromEnum mode) >>= \fd -> case fd of
    -1 -> throwErrno "fdOpen"
    otherwise -> return fd

-- | Computation 'fdClose' @fd@ makes descriptor @fd@ closed.
fdClose :: FileDescriptor -> IO ()
fdClose fd = linux_close fd >>= \rc -> case rc of
  -1 -> throwErrno "fdClose"
  otherwise -> return ()

-- | 'fdPutBuf' @fd@ @buf@ @count@ writes @count@ 8-bit bytes from the
-- buffer @buf@ to the descriptor @fd@.
fdPutBuf :: FileDescriptor -> Ptr a -> Int -> IO ()
fdPutBuf fd buf bufLen = linux_write fd buf bufLen >>= \rc -> case rc of
  -1 -> throwErrno "fdPutBuf"
  otherwise -> return ()

-- | 'fdGetBuf' @fd@ @buf@ @count@ reads data from the descriptor @fd@
-- into the buffer @buf@ until either EOF is reached or
-- @count@ 8-bit bytes have been read.
fdGetBuf :: FileDescriptor -> Ptr a -> Int -> IO ()
fdGetBuf fd buf bufLen = linux_read fd buf bufLen >>= \rc -> case rc of
  -1 -> throwErrno "fdGetBuf"
  otherwise -> return ()

