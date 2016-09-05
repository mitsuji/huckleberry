import Control.Monad(forM_)
import Control.Concurrent(threadDelay)
import System.PIO.Linux.GPIO(setValue)

{--
Connection for Intel Edison

$ sh/setup_out.sh 78

[J20-11] == +
[J19- 3] == -

--}


test = do

  forM_ [0..9] $ \_ -> do
    
    setValue 78 True
    threadDelay $ 500 * 1000
    
    setValue 78 False
    threadDelay $ 500 * 1000

  

