{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module Main where
import Relayeet

import           Conduit
import           Data.Aeson.Parser
import qualified Data.ByteString         as BS
import           Data.Conduit.Attoparsec
import           Network.HTTP.Conduit
import           Network.HTTP.Types

main :: IO ()
main = do
  Right ClientConfig{..} <- parseClientArgs
  let auth = (hAuthorization, "Bearer " <> getBearer bearer)
      Just req0 = parseRequest url
      req = req0 { requestHeaders = auth : requestHeaders req0 }
  man <- newManager tlsManagerSettings
  runResourceT $ do
    src <- responseBody <$> http req man
    runConduit $ src .| linesUnboundedAsciiC .| filterC (not . BS.null)
                     -- .| conduitParser json
                     -- .| mapC snd
                     .| mapM_C (liftIO . print)
