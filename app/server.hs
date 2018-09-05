{-# LANGUAGE DataKinds, DeriveGeneric, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeApplications #-}
{-# LANGUAGE TypeOperators                                        #-}
module Main where
import Relayeet

import           Control.Concurrent.STM
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.Aeson                as Aeson
import           Data.Text                 (Text)
import qualified Data.Text.Lazy            as LT
import qualified Data.Text.Lazy.Encoding   as LT
import           GHC.Generics
import           Network.HTTP.Types.Status
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wreq              ()
import           Servant

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

newtype Users = Users [Text]
  deriving (Read, Show, Eq, Ord, Generic, ToJSON, FromJSON)

streamApp :: Service StreamAPI
streamApp = do
  ch <- asks producer
  return $ StreamGenerator $ \sendFirst sendRest -> do
    sendFirst =<< atomically (readTChan ch)
    forever $ sendRest =<< atomically (readTChan ch)

readerToHandler :: Env -> ReaderT Env (ExceptT ServantErr IO) a -> Handler a
readerToHandler e act = do
  exc <- liftIO $ runExceptT $ runReaderT act e
  case exc of
    Right a  -> return a
    Left err -> throwError err

notFound :: Tagged (ReaderT Env (ExceptT ServantErr IO)) Application
notFound = Tagged $ \req sendResponse -> do
  putStr "Not found: "
  print req
  sendResponse $ responseLBS status404 [("Content-Type", "text/html; charset=UTF-8")] "Not Found"

server :: Env -> Server API
server env =
  hoistServer @API Proxy (readerToHandler env) $
  crcApp :<|> aaaApp :<|> streamApp

data Env = Env { config   :: Config
               , producer :: TChan Value
               }

main :: IO ()
main = do
  Right config@Config{..} <- parseArgs
  producer <- newBroadcastTChanIO
  let env = Env { .. }
  run port $ serve @API Proxy $ server env
