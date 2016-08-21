module System.PIO.Linux.PWM (
  Channel,
  Period,
  DutyCycle,
  setEnable,
  getEnable,
  setValue,
  getValue
  ) where

import System.IO (writeFile, readFile)

-- | PWM channel numbers are values of type Int
type Channel = Int

-- | PWM period is a value of type Int
type Period = Int

-- | PWM duty cycle is a value of type Int
type DutyCycle = Int


interfaceFilePath :: Int -> String -> String
interfaceFilePath n file = "/sys/class/pwm/pwmchip0/pwm" ++ (show n) ++ file

enableFilePath :: Int -> String
enableFilePath n = interfaceFilePath n "/enable"

periodFilePath :: Int -> String
periodFilePath n = interfaceFilePath n "/period"

dutyCycleFilePath :: Int -> String
dutyCycleFilePath n = interfaceFilePath n "/duty_cycle"


-- | Computation 'setEnable' @channel@ @True@ makes corresponding PWM channel enabled.
-- Computation 'setEnable' @channel@ @False@ makes corresponding PWM channel disabled.
setEnable :: Channel -> Bool -> IO ()
setEnable n flag =
  writeFile (enableFilePath n) $ show $ fromEnum flag


-- | Computation 'getEnable' @channel@ obtain status of corresponding PWM channel.
getEnable :: Channel -> IO Bool
getEnable n =
  (\flag -> toEnum $ read flag) <$> readFile (enableFilePath n)


-- | 'setValue' @channel@ @period@ @dutyCycle@ generates PWM signal on corresponding PWM channel. @period@ and @dutyCycle@ are value of nanoseconds.
setValue :: Channel -> Period -> DutyCycle -> IO ()
setValue n period dutyCycle = do
  let dutyCycleFilePath' = dutyCycleFilePath n
  writeFile dutyCycleFilePath' $ show 0
  writeFile (periodFilePath n) $ show period
  writeFile dutyCycleFilePath' $ show dutyCycle


-- | 'getValue' @channel@ obtain status of current PWM signal on corresponding PWM channel.
getValue :: Channel -> IO (Period, DutyCycle)
getValue n =
  (\p dc -> (read p, read dc)) <$> readFile (periodFilePath n) <*> readFile (dutyCycleFilePath n)


