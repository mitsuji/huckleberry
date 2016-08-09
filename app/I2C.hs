import Data.Int(Int16)
import Data.Word(Word8)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import Foreign.Ptr(Ptr)
import Foreign.Marshal.Array(allocaArray,withArrayLen,peekArray)
import Control.Monad(forM_)
import Control.Concurrent(threadDelay)

import System.PIO(hexToNum)
import System.PIO.Linux(fdOpen,oRdWr,fdClose,fdGetBuf,fdPutBuf)
import System.PIO.Linux.I2C(setSlave)


{--

Connection for Intel Edison

$ sh/setup_i2c.sh 6 27 28

[J17-7] == SCL
[J17-9] == SDA
[J19-2] == VDD
[J19-3] == GND

--}


hex :: String -> Word8
hex = hexToNum


test1 = do
  let path = "/dev/i2c-6"
  let slave = hex "1E"
  
  fd <- fdOpen path oRdWr

  setSlave fd slave

  withArrayLen [hex "00", hex "70"] $ \len p -> fdPutBuf fd p len
  withArrayLen [hex "01", hex "A0"] $ \len p -> fdPutBuf fd p len
  withArrayLen [hex "02", hex "01"] $ \len p -> fdPutBuf fd p len
  threadDelay $ 10 * 1000

  xh:xl:zh:zl:yh:yl:_ <- allocaArray 6 $
    \p -> fdGetBuf fd (p :: Ptr Word8) 6 >> peekArray 6 p
  let x = ((fromIntegral xh :: Int16) `shiftL` 8) .|. (fromIntegral xl :: Int16)
  let z = ((fromIntegral zh :: Int16) `shiftL` 8) .|. (fromIntegral zl :: Int16)
  let y = ((fromIntegral yh :: Int16) `shiftL` 8) .|. (fromIntegral yl :: Int16)
  print $ (show x) ++ ":" ++ (show y) ++ ":" ++ (show z)

  fdClose fd


test2 = do
  let path = "/dev/i2c-6"
  let slave = hex "1E"
  
  fd <- fdOpen path oRdWr

  setSlave fd slave

  withArrayLen [hex "00", hex "70"] $ \len p -> fdPutBuf fd p len
  withArrayLen [hex "01", hex "A0"] $ \len p -> fdPutBuf fd p len
  withArrayLen [hex "02", hex "00"] $ \len p -> fdPutBuf fd p len
  threadDelay $ 10 * 1000

  forM_ [0..99] $ \_ -> do
    
    withArrayLen [hex "03"] $ \len p -> fdPutBuf fd p len
    
    xh:xl:zh:zl:yh:yl:_ <- allocaArray 6 $
      \p -> fdGetBuf fd (p :: Ptr Word8) 6 >> peekArray 6 p
    let x = ((fromIntegral xh :: Int16) `shiftL` 8) .|. (fromIntegral xl :: Int16)
    let z = ((fromIntegral zh :: Int16) `shiftL` 8) .|. (fromIntegral zl :: Int16)
    let y = ((fromIntegral yh :: Int16) `shiftL` 8) .|. (fromIntegral yl :: Int16)
    print $ (show x) ++ ":" ++ (show y) ++ ":" ++ (show z)
      
    threadDelay $ 500 * 1000
    
  fdClose fd

