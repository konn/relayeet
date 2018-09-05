{-# LANGUAGE DerivingStrategies, FlexibleContexts                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, TypeFamilies #-}
module Relayeet.BearerAuth (Bearer(..), bearerHandler, loadTokens) where
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (FromJSON (..), withText)
import           Data.ByteString           (ByteString, stripPrefix)
import qualified Data.ByteString.Lazy      as LBS
import qualified Data.Text.Encoding        as T
import           Database.VCache           (PVar, VCache, VCacheable,
                                            loadRootPVar)
import           Database.VCache           (readPVarIO)
import           Network.HTTP.Types.Header (hAuthorization, hWWWAuthenticate)
import           Network.Wai               (Request, requestHeaders)
import           Servant                   (Handler, ServantErr, err401,
                                            errBody, errHeaders)
import           Servant                   (throwError)

import Servant.API.Experimental.Auth    (AuthProtect)
import Servant.Server.Experimental.Auth

newtype Bearer = Bearer { getBearer :: ByteString }
  deriving (Read, Show, Eq, Ord)
  deriving newtype (VCacheable)

instance FromJSON Bearer where
  parseJSON = withText "Bearer token string required" (pure . Bearer . T.encodeUtf8)

type instance AuthServerData (AuthProtect Bearer) = Bearer

loadTokens :: VCache -> PVar [Bearer]
loadTokens c = loadRootPVar c "tokens" []

bearerHandler :: VCache -> AuthHandler Request Bearer
bearerHandler cache = mkAuthHandler $ \req -> do
  liftIO $ putStrLn $ "Authing: " <> show req
  let hdrs = requestHeaders req
  case lookup hAuthorization hdrs of
    Just autho ->
      case stripPrefix "Bearer " autho of
        Just b -> do
          bs <- liftIO $ readPVarIO (loadTokens cache)
          if Bearer b `elem` bs
            then return $ Bearer b
            else report401 "invalid_token"
        Nothing -> do
          liftIO $ putStrLn $ "No bearer: " <> show hdrs
          throwError err401 { errBody = "No bearer found"
                            , errHeaders = [(hWWWAuthenticate, "Bearer realm=\"stream\"")]
                            }
    Nothing -> throwError err401 { errHeaders = [(hWWWAuthenticate, "Bearer realm=\"stream\"")]
                                 , errBody = "You have to be authorized"
                                 }

report401 :: ByteString -> Handler a
report401 str = withWWWAuth err401 { errBody = LBS.fromStrict str } str

withWWWAuth :: ServantErr -> ByteString -> Handler a
withWWWAuth err str =
  throwError err  { errHeaders = [(hWWWAuthenticate, "Bearer error=\"" <> str <> "\"")] }
