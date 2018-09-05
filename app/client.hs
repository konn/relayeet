module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.Mem.Weak

withFinalizer :: Integer -> IO (Weak Integer)
withFinalizer i = mkWeakPtr i (Just $ putStrLn $ unwords ["released: ", show i])

main :: IO ()
main = do
  tc <- newBroadcastTChanIO
  forM_ [0 ..] $ atomically . writeTChan tc <=< withFinalizer
  forever $ threadDelay maxBound
