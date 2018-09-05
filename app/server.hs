{-# LANGUAGE DataKinds, OverloadedStrings, RecordWildCards #-}
{-# LANGUAGE TypeApplications, TypeOperators               #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where
import Relayeet

import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson                       as Aeson
import           Data.Aeson.Text
import qualified Data.Text.Lazy.Encoding          as LT
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant.Server.Experimental.Auth

import Servant

type Service a = ServerT a (ReaderT Env (ExceptT ServantErr IO))

crcApp :: Service CrcAPI
crcApp Nothing = throwError $ err400 {errBody = "No CRC Token Provided"}
crcApp (Just (CRCToken token)) = do
  secret <- asks $ consumerSecret . config
  return $ WebhookSignature $ encodeKeyVal secret token

aaaApp :: Service AAAAPI
aaaApp Nothing _ = throwError $ err400 {errBody = "No webhook signature provided"}
aaaApp (Just (WebhookSignature sig)) body =
  let src = LT.encodeUtf8 body
  in case Aeson.decode src of
       Nothing -> throwError $ err400 { errBody = "Input must be a valid JSON" }
       Just v -> do
         secret <- asks $ consumerSecret . config
         unless (encodeKeyVal secret src == sig) $
           throwError $ err400 {errBody = "Webhook signature mismatched"}
         chan <- asks producer
         liftIO $ atomically $ writeTChan chan v
         return NoContent

streamApp :: Service StreamAPI
streamApp (Bearer b) = do
  liftIO $ putStrLn $ "Streaming to: " <> show b
  ch <- liftIO . atomically . dupTChan =<< asks producer
  let readOr = maybe "" encodeToLazyText . either id id <$>
               atomically (readTChan ch) `race` do threadDelay (10^7); return Nothing
  return $ StreamGenerator $ \sendFirst sendRest -> do
    sendFirst =<< readOr
    forever $ sendRest =<< readOr

readerToHandler :: Env -> ReaderT Env (ExceptT ServantErr IO) a -> Handler a
readerToHandler e act = do
  exc <- liftIO $ runExceptT $ runReaderT act e
  case exc of
    Right a  -> return a
    Left err -> throwError err

server :: Env -> Server API
server env =
  hoistServerWithContext @API @'[AuthHandler Request Bearer] Proxy Proxy (readerToHandler env) $
  crcApp :<|> aaaApp :<|> streamApp

data Env = Env { config   :: Config
               , producer :: TChan (Maybe Value)
               }

main :: IO ()
main = do
  Right config@Config{..} <- parseServerArgs
  producer <- newBroadcastTChanIO
  vcache   <- sharedVCache
  let env = Env { .. }
      cfg = bearerHandler vcache :. EmptyContext
  run port $ serveWithContext @API Proxy cfg (server env)
