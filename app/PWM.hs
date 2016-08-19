import Control.Monad(forM_)
import Control.Concurrent(threadDelay)
import System.PIO.Linux.PWM(setValue)

{--
Connection for Intel Edison

$ sh/setup_pwm.sh 0 1 13

[J18- 1] == +
[J19- 3] == -

--}


test = do

  forM_ [0..9] $ \_ -> do
    
    setValue 1 200000 200000
    threadDelay $ 500 * 1000
    
    setValue 1 200000 50000
    threadDelay $ 500 * 1000

  

