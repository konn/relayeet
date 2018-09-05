module Main where
import Relayeet

import Crypto.Nonce
import Crypto.PasswordStore
import Database.VCache

main :: IO ()
main = withGenerator $ \gen -> do
  nonce <- nonce128url gen
  hashed <- makePassword nonce 17
  cache <- sharedVCache
  let toks = loadTokens cache
  runVTx (vcache_space cache) $ modifyPVar' toks (Bearer hashed:)
  putStrLn $ "Registered: " ++ show nonce
  vcacheSync $ vcache_space cache
