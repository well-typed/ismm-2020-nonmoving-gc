{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

#define EVENT_LOG

module BenchUtils where

import Control.Monad.IO.Class
import System.Clock
import Debug.Trace

startMarker :: String -> IO ()
startMarker lbl = traceMarkerIO ("start:"<>lbl<>":")

endMarker :: String -> IO ()
endMarker lbl = traceMarkerIO ("end:"<>lbl<>":")

timeIt :: MonadIO m => String -> m a -> m a
#ifdef EVENT_LOG
timeIt label action = do
    liftIO $ startMarker label
    !r <- action
    liftIO $ endMarker label
    return r
#else
timeIt label action = do
    start <- liftIO $ getTime Monotonic
    !r <- action
    end <- liftIO $ getTime Monotonic
    let dt = toNanoSecs $ end `diffTimeSpec` start
    liftIO $ putStrLn $ label <> "\t" <> show (realToFrac dt * 1e-9)
    return r
#endif
