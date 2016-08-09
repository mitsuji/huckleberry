module System.PIO.Linux.GPIO (
  setValue,
  getValue
  ) where

import System.IO (writeFile, readFile)

  
valueFilePath :: Int -> String
valueFilePath n = "/sys/class/gpio/gpio" ++ (show n) ++ "/value"


setValue :: Int -> Bool -> IO ()
setValue n flag =
  writeFile (valueFilePath n) $ show $ fromEnum flag


getValue :: Int -> IO Bool
getValue n =
  (\flag -> toEnum $ read flag) <$> readFile (valueFilePath n)

