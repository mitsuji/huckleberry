import Data.Word(Word8,Word16)
import Data.Bits((.|.),(.&.),shiftL,shiftR)
import Foreign.Marshal.Array(withArrayLen,peekArray)
import Control.Monad(forM_)
import Control.Concurrent(threadDelay)

import System.PIO(binToNum)
import System.PIO.Linux(fdOpen,oRdWr,fdClose)
import System.PIO.Linux.SPI


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


test = do
  let path = "/dev/spidev5.1"
  let mode = mode0
  let bits = 8
  let speed = 1000000
  let ch = 0
  
  fd <- fdOpen path oRdWr
  
  setWrMode fd mode
  setRdMode fd mode
  
  setWrBitsPerWord fd bits
  setRdBitsPerWord fd bits
  
  setWrMaxSpeedHz fd speed
  setRdMaxSpeedHz fd speed


  forM_ [0..99] $ \_ -> do
    
    let dt0 = bin "1100000" .|. (ch `shiftL` 4)
    let dt1 = 0
    dt0':dt1':_ <- withArrayLen [dt0,dt1] $
      \len p -> transferMessage1 fd p len speed bits >> peekArray len p
      
    let v = ((fromIntegral (dt0' .&. bin "11") :: Word16) `shiftL` 8) .|. (fromIntegral dt1' :: Word16)
    print v
      
    threadDelay $ 500 * 1000

  fdClose fd



dt0'' ch = bin "00000110" .|. ( (bin "0100" .&. ch) `shiftR` 2)
dt1'' ch = bin "00000000" .|. ( (bin "0011" .&. ch) `shiftL` 6)

    
