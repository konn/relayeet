module Main where
import Relayeet

import Crypto.Nonce
import Database.VCache

main :: IO ()
main = withGenerator $ \gen -> do
  nonce <- nonce128url gen
  cache <- sharedVCache
  let toks = loadTokens cache
  runVTx (vcache_space cache) $ modifyPVar' toks (Bearer nonce:)
  putStrLn $ "Registered: " ++ show nonce
