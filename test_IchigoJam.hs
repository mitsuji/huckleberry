import qualified IchigoJam as IJ

port = "/dev/ttyUSB0"


test = do
  h <- IJ.opend port
  
  IJ.send h "PRINT \"YEAH!!\"\n"
  putStrLn =<< IJ.recv h 10

  IJ.send h "LED 1\n"
  putStrLn =<< IJ.recv h 10
  
--  IJ.send h "PLAY \"CDEFGAB\"\n"
--  putStrLn =<< IJ.recv h 10
  
  IJ.send h "LED 0\n"
  putStrLn =<< IJ.recv h 10
  
  IJ.send h "PRINT BTN ()\n"
  putStrLn =<< IJ.recv h 10


  IJ.led h True
--  IJ.play h "CDEFGAB"
  IJ.led h False
  b <- IJ.btn h


  IJ.close h


