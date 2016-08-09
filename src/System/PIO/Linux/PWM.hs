module System.PIO.Linux.PWM (
  setEnable,
  getEnable,
  setValue,
  getValue
  ) where

import System.IO (writeFile, readFile)


interfaceFilePath :: Int -> String -> String
interfaceFilePath n file = "/sys/class/pwm/pwmchip0/pwm" ++ (show n) ++ file

enableFilePath :: Int -> String
enableFilePath n = interfaceFilePath n "/enable"

periodFilePath :: Int -> String
periodFilePath n = interfaceFilePath n "/period"

dutyCycleFilePath :: Int -> String
dutyCycleFilePath n = interfaceFilePath n "/duty_cycle"


setEnable :: Int -> Bool -> IO ()
setEnable n flag =
  writeFile (enableFilePath n) $ show $ fromEnum flag


getEnable :: Int -> IO Bool
getEnable n =
  (\flag -> toEnum $ read flag) <$> readFile (enableFilePath n)


setValue :: Int -> Int -> Int -> IO ()
setValue n period dutyCycle = do
  let dutyCycleFilePath' = dutyCycleFilePath n
  writeFile dutyCycleFilePath' $ show 0
  writeFile (periodFilePath n) $ show period
  writeFile dutyCycleFilePath' $ show dutyCycle


getValue :: Int -> IO (Int, Int)
getValue n =
  (\p dc -> (read p, read dc)) <$> readFile (periodFilePath n) <*> readFile (dutyCycleFilePath n)


