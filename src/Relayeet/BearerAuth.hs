{-# LANGUAGE DerivingStrategies, FlexibleContexts                        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, OverloadedStrings, TypeFamilies #-}
module Relayeet.BearerAuth (Bearer(..), bearerHandler, loadTokens) where
import           Control.Monad.IO.Class    (liftIO)
import           Data.Aeson                (FromJSON (..), withText)
import           Data.ByteString           (ByteString, stripPrefix)
import qualified Data.Text.Encoding        as T
import           Database.VCache           (PVar, VCache, VCacheable,
                                            loadRootPVar)
import           Database.VCache           (readPVarIO)
import           Network.HTTP.Types.Header (hAuthorization, hWWWAuthenticate)
import           Network.Wai               (Request, requestHeaders)
import           Servant                   (Handler, ServantErr, err401,
                                            errHeaders)
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
  let hdrs = requestHeaders req
  case lookup hAuthorization hdrs of
    Just autho ->
      case stripPrefix "Bearer " autho of
        Just b -> do
          bs <- liftIO $ readPVarIO (loadTokens cache)
          if Bearer b `elem` bs
            then return $ Bearer b
            else report401 "invalid_token"
        Nothing -> throwError err401 { errHeaders = [(hWWWAuthenticate, "Bearer realm=\"stream\"")] }
    Nothing -> throwError err401 { errHeaders = [(hWWWAuthenticate, "Bearer realm=\"stream\"")] }

report401 :: ByteString -> Handler a
report401 = withWWWAuth err401

withWWWAuth :: ServantErr -> ByteString -> Handler a
withWWWAuth err str =
  throwError err  { errHeaders = [(hWWWAuthenticate, "Bearer error=\"" <> str <> "\"")] }
