module System.PIO.Linux.GPIO (
  PinNumber,
  setValue,
  getValue
  ) where

import System.IO (writeFile, readFile)

-- | GPIO pin numbers are values of type Int
type PinNumber = Int

valueFilePath :: Int -> String
valueFilePath n = "/sys/class/gpio/gpio" ++ (show n) ++ "/value"


-- | Computation 'setValue' @pinnum@ @True@ makes corresponding pin high.
-- Computation 'setValue' @pinnum@ @False@ makes corresponding pin low.
setValue :: PinNumber -> Bool -> IO ()
setValue n flag =
  writeFile (valueFilePath n) $ show $ fromEnum flag


-- | Computation 'getValue' @pinnum@ obtain status of corresponding pin.
getValue :: PinNumber -> IO Bool
getValue n =
  (\flag -> toEnum $ read flag) <$> readFile (valueFilePath n)

