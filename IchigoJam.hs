module IchigoJam (
  Handle(Handle)
  ,open
  ,opend
  ,close
  ,send
  ,recv
  ,led
  ,play
  ,btn
  ) where


import qualified Data.ByteString.Char8 as B
import qualified System.Hardware.Serialport as S
import Control.Monad(forM_,foldM,forever)
import Control.Concurrent(ThreadId,forkIO,killThread,threadDelay)




led :: Handle -> Bool -> IO ()
led h on = do
  let flag = case on of
        True -> "1"
        False -> "0"
  send h $ "LED " ++ flag ++ "\n"
  r <- recv h 4
  return ()


play :: Handle -> String -> IO ()
play h score = do
  send h $ "PLAY \"" ++ score ++ "\"\n"
  r <- recv h 4
  return ()
  

btn :: Handle -> IO Bool
btn h = do
  send h $ "PRINT BTN ()\n"
  r <- recv h 4 -- [TODO] convert to Bool
  putStrLn r
  return True





defaultSendDelay = 50 * 1000
defaultRecvDelay = 50 * 1000


data Handle = Handle
  { port :: S.SerialPort
  , sendDelay :: Int
  , recvDelay :: Int
  }


open :: FilePath -> Int -> Int -> IO Handle
open path sd rd = do
  p <- S.openSerial path S.defaultSerialSettings {
    S.commSpeed = S.CS115200
    }
  return $ Handle p sd rd

opend path = open path defaultSendDelay defaultRecvDelay


close :: Handle -> IO()
close (Handle p sd rd) = do
  S.closeSerial p


send :: Handle -> String -> IO()
send (Handle p sd _) s = do
  forM_ s (\c -> do
              S.send p $ B.singleton c
              S.flush p
              threadDelay sd
          )


recv :: Handle -> Int -> IO String
recv (Handle p _ rd) l = do
  bs <- foldM( \acc _-> do
                  b <- S.recv p 1
                  threadDelay rd
                  return $ acc `B.append` b
             ) B.empty [1..l]
  return $ B.unpack bs

