{-# LANGUAGE DataKinds, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TypeApplications, TypeOperators               #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where
import Relayeet

import           Conduit                          (repeatMC, yield)
import           Control.Concurrent               hiding (yield)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.Aeson.Text
import qualified Data.ByteString.Lazy.Char8       as LBS
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant.Conduit
import           Servant.Server.Experimental.Auth
import           Servant.Types.SourceT

import Servant

type Service a = ServerT a (ReaderT Env (ExceptT ServerError IO))

crcApp :: Service CrcAPI
crcApp Nothing = throwError $ err400 {errBody = "No CRC Token Provided"}
crcApp (Just (CRCToken token)) = do
  secret <- asks $ consumerSecret . config
  return $ WebhookSignature $ encodeKeyVal secret token

aaaApp :: Service AAAAPI
aaaApp Nothing _ = throwError $ err400 {errBody = "No webhook signature provided"}
aaaApp (Just (WebhookSignature sig)) (WithSource src val) = do
  secret <- asks $ consumerSecret . config
  unless (encodeKeyVal secret src == sig) $ do
    liftIO $ putStr "Sign unmatched..."
    throwError $ err400 {errBody = "Webhook signature mismatched"}
  chan <- asks producer
  liftIO $ atomically $ writeTChan chan (Just val)
  return NoContent

streamApp :: Service StreamAPI
streamApp (Bearer b) = do
  liftIO $ putStrLn $ "Streaming to: " <> show b
  ch <- liftIO . atomically . dupTChan =<< asks producer
  let readOr = either id id <$>
               atomically (readTChan ch) `race` do threadDelay (5*10^6); return Nothing
  return $ conduitToSourceIO $ repeatMC $ encodeToLazyText <$> readOr

readerToHandler :: Env -> ReaderT Env (ExceptT ServerError IO) a -> Handler a
readerToHandler e act = do
  exc <- liftIO $ runExceptT $ runReaderT act e
  case exc of
    Right a  -> return a
    Left err -> throwError err

server :: Env -> Server (API :<|> Raw)
server env =
  hoistServerWithContext @(API :<|> Raw) @'[AuthHandler Request Bearer] Proxy Proxy (readerToHandler env) $
  (crcApp :<|> aaaApp :<|> streamApp) :<|> notFound

data Env = Env { config   :: Config
               , producer :: TChan (Maybe Value)
               }

notFound :: Tagged (ReaderT Env (ExceptT ServerError IO)) Application
notFound = Tagged $ \req sendResponse -> do
  liftIO $ putStrLn $ "Not Found: " ++ show req
  sendResponse $ responseLBS status404 [] $ "Not Found: " <> LBS.pack (show $ rawPathInfo req)

main :: IO ()
main = do
  Right config@Config{..} <- parseServerArgs
  producer <- newBroadcastTChanIO
  vcache   <- sharedVCache
  let env = Env { .. }
      cfg = bearerHandler vcache :. EmptyContext
  run port $ serveWithContext @(API :<|> Raw) Proxy cfg (server env)
