{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE DataKinds, DeriveAnyClass, DeriveFunctor, DeriveGeneric     #-}
{-# LANGUAGE DerivingStrategies, ExtendedDefaultRules, FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, MultiParamTypeClasses           #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, ScopedTypeVariables     #-}
{-# LANGUAGE TypeFamilies, TypeOperators, TypeSynonymInstances           #-}
module Relayeet
  ( Config, Config'(..), Token(..), CRCToken(..)
  , WebhookSignature(..), OAuthVerification(..)
  , encodeKeyVal, bearerTokenCredential
  , CrcAPI, StreamAPI, AAAAPI, API, OAuthCallbackAPI
  , parseServerArgs, sharedVCache, ClientConfig(..)
  , parseClientArgs, WithSource(..), MuteSettings(..)
  , module Relayeet.BearerAuth
  , module Relayeet.Activity
  ) where
import Relayeet.Activity
import Relayeet.BearerAuth

import Data.Aeson (FromJSON (..), ToJSON (..), camelTo2)

import           Data.Aeson                  (defaultOptions, (.=), genericToJSON)
import           Data.Aeson                  (Value, fieldLabelModifier)
import           Data.Aeson                  (genericParseJSON, object, Options)
import qualified Data.ByteString.Base64      as B64
import qualified Data.ByteString.Base64.Lazy as LB64
import qualified Data.ByteString.Char8       as BS
import qualified Data.ByteString.Lazy.Char8  as LBS
import           Data.Digest.Pure.SHA        (bytestringDigest, hmacSha256)
import           Data.List.NonEmpty          (NonEmpty)
import           Data.Maybe                  (fromMaybe, listToMaybe)
import qualified Data.Text                   as T
import qualified Data.Text.Encoding          as T
import qualified Data.Text.Lazy              as LT
import qualified Data.Text.Lazy.Encoding     as LT
import           Data.Yaml                   (ParseException, decodeFileEither)
import           Database.VCache             (VCache, VCacheable, openVCache)
import           GHC.Generics                (Generic)
import           Network.HTTP.Types.URI      (urlEncode)
import           Servant.API                 ((:<|>), (:>), AuthProtect, JSON)
import           Servant.API                 (MimeRender (..))
import           Servant.API                 (ToHttpApiData (..))
import           Servant.API                 (FromHttpApiData (..), Get, Header)
import           Servant.API                 (NewlineFraming, NoContent)
import           Servant.API                 (PlainText, Post, QueryParam)
import           Servant.API                 (ReqBody, StreamGenerator)
import           Servant.API                 (StreamGet)
import           Servant.API.ContentTypes    (MimeUnrender (..))
import           System.Environment          (getArgs)

default (T.Text)

data Config' tok = Config { address           :: String
                          , port              :: Int
                          , endpoint          :: T.Text
                          , consumerKey       :: tok
                          , consumerSecret    :: tok
                          , accessToken       :: tok
                          , accessTokenSecret :: tok
                          , environment       :: String
                          , accounts          :: NonEmpty String
                          , oauthCallback     :: tok
                          , oauthCallbackPort :: Int
                          , oauthBearer       :: Maybe tok
                          }
                 deriving (Read, Show, Eq, Ord, Generic, Functor)
type Config = Config' BS.ByteString

instance FromJSON (Config' Token) where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' }

instance FromJSON Config where
  parseJSON = fmap (fmap runToken) . parseJSON

instance FromJSON Token where
  parseJSON = fmap (Token . T.encodeUtf8) . parseJSON

instance FromHttpApiData Token where
  parseUrlPiece = Right . Token . T.encodeUtf8
  parseQueryParam = Right . Token . T.encodeUtf8

bearerTokenCredential :: Config -> BS.ByteString
bearerTokenCredential Config{..} =
  let key = urlEncode True consumerKey
      sec = urlEncode True consumerSecret
  in B64.encode $ key <> ":" <> sec

encodeKeyVal :: BS.ByteString -> LBS.ByteString -> LBS.ByteString
encodeKeyVal key val = bytestringDigest $ hmacSha256 (LBS.fromStrict key) val

newtype WebhookSignature = WebhookSignature { responseToken :: LBS.ByteString }
  deriving (Read, Show, Eq, Ord, Generic)

instance ToJSON WebhookSignature where
  toJSON (WebhookSignature sig) =
    object [ "response_token" .= ("sha256=" <> LT.decodeUtf8 (LB64.encode sig)) ]

newtype CRCToken  = CRCToken { runCRCToken :: LBS.ByteString }
  deriving (Read, Show, Eq, Ord)

instance FromHttpApiData CRCToken where
  parseQueryParam = Right . CRCToken . LBS.fromStrict . T.encodeUtf8

type OAuthCallbackAPI =
     "oauth_callback"
  :> QueryParam "oauth_token" Token
  :> QueryParam "oauth_verifier" Token
  :> Get '[PlainText] T.Text

data OAuthVerification = OAuthVerification

type CrcAPI = "activity"
           :> QueryParam "crc_token" CRCToken :> Get '[JSON] WebhookSignature
type AAAAPI = "activity"
           :> Header "X-Twitter-Webhooks-Signature" WebhookSignature
           :> ReqBody '[JSON] (WithSource Value)
           :> Post '[PlainText] NoContent
type StreamAPI = "stream"
              :> AuthProtect Bearer
              :> StreamGet NewlineFraming PlainText (StreamGenerator LT.Text)

data WithSource a = WithSource { rawSource :: LBS.ByteString, payload :: a }
  deriving (Read, Show, Eq, Ord)

instance {-# OVERLAPPING #-} MimeUnrender JSON a => MimeUnrender JSON (WithSource a) where
  mimeUnrender pxy src =
    case mimeUnrender pxy src of
      Left err -> Left err
      Right a  -> Right $ WithSource src a

newtype Token = Token { runToken :: BS.ByteString }
  deriving (Read, Show, Eq, Ord)
  deriving newtype (VCacheable)

type API = CrcAPI :<|> AAAAPI :<|> StreamAPI

renderWebhookSig :: WebhookSignature -> BS.ByteString
renderWebhookSig (WebhookSignature sig) = "sha256=" <> LBS.toStrict (LB64.encode sig)

parseWebhookSig :: BS.ByteString -> Either T.Text WebhookSignature
parseWebhookSig input =
  case BS.stripPrefix "sha256=" input of
    Just code -> either (Left . T.pack) (Right . WebhookSignature . LBS.fromStrict) $ B64.decode code
    Nothing -> Left "No prefix sha256= found"

instance FromHttpApiData WebhookSignature where
  parseQueryParam = parseWebhookSig . T.encodeUtf8
  parseHeader = parseWebhookSig

instance MimeRender PlainText WebhookSignature where
  mimeRender _  = LBS.fromStrict . renderWebhookSig

instance ToHttpApiData WebhookSignature where
  toQueryParam = T.decodeUtf8 . renderWebhookSig
  toHeader = renderWebhookSig

parseServerArgs :: IO (Either ParseException Config)
parseServerArgs = do
  fp <- fromMaybe "config/server.yaml" . listToMaybe <$> getArgs
  decodeFileEither fp

data MuteSettings = MuteSettings { muteKeywords :: [T.Text] 
                                 , muteUsers    :: [T.Text]
                                 }
  deriving (Read, Show, Eq, Ord, Generic)

msOpts :: Options
msOpts = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 4 }

instance ToJSON MuteSettings where
  toJSON = genericToJSON msOpts

instance FromJSON MuteSettings where
  parseJSON = genericParseJSON msOpts

data ClientConfig = ClientConfig { bearer  :: Bearer
                                 , url     :: String
                                 , targets :: [T.Text]
                                 , mute    :: MuteSettings
                                 }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving anyclass (FromJSON)

parseClientArgs :: IO (Either ParseException ClientConfig)
parseClientArgs = do
  fp <- fromMaybe "config/client.yaml" . listToMaybe <$> getArgs
  decodeFileEither fp

sharedVCache :: IO VCache
sharedVCache = openVCache 100 "_vcache"
