import Data.Word(Word8,Word16)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import Foreign.Marshal.Array(withArrayLen,peekArray)
import Control.Monad(forM_)
import Control.Concurrent(threadDelay)

import System.PIO(binToNum)
import System.PIO.Linux(fdOpen,IOMode(ReadWriteMode),fdClose)
import System.PIO.Linux.SPI.Raw


{--

Connection for Intel Edison

$ sh/setup_spi.sh 5 1 109 114 115 111

[J17-10] == SS
[J17-11] == SCLK
[J17-12] == MOSI
[J18-11] == MISO
[J19- 2] == VDD
[J19- 3] == GND

--}


bin :: String -> Word8
bin = binToNum


demoSetting :: IO ()
demoSetting = do

  fd <- fdOpen "/dev/spidev5.1" ReadWriteMode
  
  print . fromEnum =<< getMode fd
  
  setMode fd Mode1
  print . fromEnum =<< getMode fd
  
  setMode fd Mode2
  print . fromEnum =<< getMode fd
  
  setMode fd Mode3
  print . fromEnum =<< getMode fd
  
  setMode fd Mode0
  print . fromEnum =<< getMode fd
  
  
  print =<< getLsbFirst fd
  
--  setLsbFirst fd True
--  print =<< getLsbFirst fd
  
  setLsbFirst fd False
  print =<< getLsbFirst fd

  
  print =<< getBitsPerWord fd
  
  setBitsPerWord fd 7
  print =<< getBitsPerWord fd
  
  setBitsPerWord fd 8
  print =<< getBitsPerWord fd
  
  setBitsPerWord fd 10
  print =<< getBitsPerWord fd

  setBitsPerWord fd 20
  print =<< getBitsPerWord fd


  print =<< getMaxSpeedHz fd
  
  setMaxSpeedHz fd 7000
  print =<< getMaxSpeedHz fd
  
  setMaxSpeedHz fd 8000
  print =<< getMaxSpeedHz fd
  
  setMaxSpeedHz fd 9100
  print =<< getMaxSpeedHz fd
  
  setMaxSpeedHz fd 1000000
  print =<< getMaxSpeedHz fd
  

  fdClose fd



demoTransferTxRx1 :: IO ()
demoTransferTxRx1 = do
  
  let bits = 8
  let speed = 1000000
  let ch = 0
  
  fd <- fdOpen "/dev/spidev5.1" ReadWriteMode
  
  setMode fd Mode0
  setBitsPerWord fd bits
  setMaxSpeedHz fd speed

  forM_ [0..99] $ \_ -> do
    
    let dt0 = bin "1100000" .|. (ch `shiftL` 4)
    let dt1 = 0
    dt0':dt1':_ <- withArrayLen [dt0,dt1] $
      \len p -> transferTxRx1 fd p len bits speed 0 False >> peekArray len p
      
    let v = ((fromIntegral (dt0' .&. bin "11") :: Word16) `shiftL` 8) .|. (fromIntegral dt1' :: Word16)
    print v
      
    threadDelay $ 500 * 1000

  fdClose fd



dt0'' ch = bin "00000110" .|. ( (bin "0100" .&. ch) `shiftR` 2)
dt1'' ch = bin "00000000" .|. ( (bin "0011" .&. ch) `shiftL` 6)

    

