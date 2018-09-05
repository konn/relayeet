{-# LANGUAGE DataKinds, DeriveFunctor, DeriveGeneric, DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, TypeApplications, TypeFamilies             #-}
{-# LANGUAGE TypeOperators, TypeSynonymInstances                         #-}
module Relayeet
  ( Config, Config'(..), Token(..), CRCToken(..)
  , WebhookSignature(..), OAuthVerification(..)
  , encodeKeyVal, bearerTokenCredential
  , CrcAPI, StreamAPI, AAAAPI, API, OAuthCallbackAPI
  , parseArgs
  ) where
import           Data.Aeson                  (FromJSON (..), Value, camelTo2)
import           Data.Aeson                  (defaultOptions)
import           Data.Aeson                  (fieldLabelModifier)
import           Data.Aeson                  (genericParseJSON)
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
import           Data.Yaml                   (ParseException, decodeFileEither)
import           GHC.Generics                (Generic)
import           Network.HTTP.Types.URI      (urlEncode)
import           Servant.API                 ((:<|>), (:>), MimeRender (..),
                                              ToHttpApiData (..))
import           Servant.API                 (FromHttpApiData (..), Get, Header)
import           Servant.API                 (JSON, NewlineFraming, NoContent)
import           Servant.API                 (PlainText, Post, QueryParam)
import           Servant.API                 (ReqBody, StreamGenerator)
import           Servant.API                 (StreamGet)
import           System.Environment          (getArgs)

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

newtype WebhookSignature = WebhookSignature LBS.ByteString
  deriving (Read, Show, Eq, Ord)

newtype CRCToken  = CRCToken { runCRCToken :: LBS.ByteString }
  deriving (Read, Show, Eq, Ord)

instance FromHttpApiData CRCToken where
  parseQueryParam = Right . CRCToken . LBS.fromStrict . T.encodeUtf8

type OAuthCallbackAPI =
     "oauth_callback"
  :> QueryParam "oauth_token" Token
  :> QueryParam "oauth_verifier" Token
  :> Post '[PlainText] NoContent

data OAuthVerification = OAuthVerification

type CrcAPI = "activity"
           :> QueryParam "crc_token" CRCToken :> Get '[PlainText] WebhookSignature
type AAAAPI = "activity"
           :> Header "x-twitter-webhooks-signature" WebhookSignature
           :> ReqBody '[PlainText] LT.Text
           :> Post '[PlainText] NoContent
type StreamAPI = "stream" :> StreamGet NewlineFraming JSON (StreamGenerator Value)

newtype Token = Token { runToken :: BS.ByteString }
  deriving (Read, Show, Eq, Ord)

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

parseArgs :: IO (Either ParseException Config)
parseArgs = do
  fp <- fromMaybe "config.yaml" . listToMaybe <$> getArgs
  decodeFileEither @Config fp


