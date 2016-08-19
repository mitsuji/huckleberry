import Control.Monad(forM_)
import Control.Concurrent(threadDelay)
import System.PIO.Linux.GPIO(getValue)

{--
Connection for Intel Edison

$ sh/setup_in.sh 78

[J20-11]
[J19- 2]

--}


test = do

  forM_ [0..999] $ \_ -> do
    
    print =<< getValue 78
    threadDelay $ 100 * 1000

