{-# LANGUAGE RecordWildCards, TypeApplications #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where
import Relayeet

import           Control.Concurrent
import           Control.Monad
import qualified Data.Aeson              as Aeson
import           Data.Proxy              (Proxy (..))
import qualified Data.Text.Lazy.Encoding as LT
import           Network.HTTP.Client     (defaultManagerSettings, newManager)
import           Servant.Client

main :: IO ()
main = do
  Right Config{..} <- parseServerArgs
  let sendActivity = client @AAAAPI Proxy
  man <- newManager defaultManagerSettings
  let env = mkClientEnv man (BaseUrl Http "localhost" 5289 "")
  forM_ [1..] $ \i -> do
    let src = Aeson.encode (i :: Integer)
        withSrc = WithSource src $ Aeson.toJSON i
        sig = WebhookSignature $ encodeKeyVal consumerSecret src
    print =<< runClientM (sendActivity (Just sig) withSrc) env
    threadDelay (10^5)
