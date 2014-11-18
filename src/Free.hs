module Free where
import Control.Monad.Trans
import Control.Monad.Trans.Iter

untilSuccess :: Monad m => m (Maybe a) -> IterT m a
untilSuccess f = maybe (delay (untilSuccess f)) return =<< lift f

data TargetHit
launchMissles :: IO (Maybe TargetHit)
launchMissles = putStrLn "Launching..." >> return Nothing

tryAndLaunchMissiles :: IO (Maybe TargetHit)
tryAndLaunchMissiles = retract (cutoff 10 $ untilSuccess launchMissles)

