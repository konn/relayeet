{-# LANGUAGE DeriveGeneric, ExtendedDefaultRules, LambdaCase      #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards, TypeApplications #-}
{-# LANGUAGE TypeOperators                                        #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where
import Relayeet

import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson                as Ae
import qualified Data.ByteString.Char8     as BS
import           Data.Hashable             (Hashable)
import           Data.Maybe
import           Data.Text                 (Text)
import qualified Data.Text.Encoding        as T
import           GHC.Generics              (Generic)
import           Network.HTTP.Client       hiding (Proxy, queryString)
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.Header
import           Network.Wai.Handler.Warp
import           Network.Wreq              (FormParam, auth, defaults,
                                            oauth1Auth, postWith)
import           Servant                   hiding (header)
import qualified STMContainers.Map         as TMap
import           Web.Authenticate.OAuth    as OA

default (Text)

type TMap = TMap.Map

configToOAuth :: Config -> OAuth
configToOAuth Config{oauthCallback = cb, ..} =
  newOAuth { oauthServerName = "Twitter"
           , oauthRequestUri = "https://api.twitter.com/oauth/request_token"
           , oauthAccessTokenUri = "https://api.twitter.com/oauth/access_token"
           , oauthAuthorizeUri = "https://api.twitter.com/oauth/authorize"
           , oauthConsumerKey = consumerKey
           , oauthConsumerSecret = consumerSecret
           , OA.oauthCallback = Just cb
           }

configToCred :: Config -> Credential
configToCred Config{..} = newCredential accessToken accessTokenSecret

data BearerResponse = BearerResponse { brTokenType   :: String
                                     , brAccessToken :: Token
                                     }
  deriving (Read, Show, Eq, Ord, Generic)

brOpts :: Ae.Options
brOpts = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

instance FromJSON BearerResponse where
  parseJSON = genericParseJSON brOpts

aaaEndpoint :: Config -> String
aaaEndpoint Config{..} =
  mconcat [ "https://api.twitter.com/1.1/account_activity/all/"
          , environment
          ]

registerWebhook :: Config -> IO ()
registerWebhook cfg@Config{..} = do
  man <- newManager tlsManagerSettings
  let url = parseRequest_ $ mconcat [aaaEndpoint cfg, "/webhooks.json" ]
      params = [("url", Just $ T.encodeUtf8 endpoint)]
  cfm <- httpLbs url { requestHeaders = (hAuthorization, "Bearer " <> fromJust oauthBearer) : requestHeaders url } man

  let there = maybe False (any @[] ((== endpoint) . whUrl)) $
              decode' $ responseBody cfm
  if there
    then putStrLn "Webhook already exists; skipped."
    else do
    req <- signOAuth (configToOAuth cfg) (configToCred cfg) (setQueryString params url { method = "POST" })
    void $ httpLbs req man

data Webhook = Webhook { whId               :: Text
                       , whUrl              :: Text
                       , whValid            :: Bool
                       , whCreatedTimestamp :: Text
                       }
  deriving (Read, Show, Eq, Ord, Generic)

instance FromJSON Webhook where
  parseJSON = genericParseJSON defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 2 }

data Env = Env { tempCredDic :: TMap String Credential
               , tokToUser   :: TMap BS.ByteString String
               , accTokDic   :: TMap String Credential
               }

type TokenDic = TMap String (BS.ByteString, BS.ByteString)

registerUser :: Env -> Config -> String -> IO ()
registerUser Env{..} cfg@Config{..} account = do
  let oauth = configToOAuth cfg
  man <- newManager tlsManagerSettings
  tmpCreds <- getTemporaryCredential oauth man
  atomically $ do
    TMap.insert tmpCreds account tempCredDic
    TMap.insert account (fromJust $ lookup "oauth_token" $ unCredential tmpCreds) tokToUser
  let paras = [("force_login", "true"), ("scree_name", BS.pack account)]
  putStrLn $ mconcat [ "Open: "
                     , authorizeUrl' (\_ _ -> paras) oauth tmpCreds
                     ]
  cred <- atomically $ lookupTMap' account accTokDic
  let reqUrl = aaaEndpoint cfg ++ "/subscriptions.json"
      (accTok, accTokSec) = (lookupAccessToken cred, lookupAccessTokenSecret cred)
      opts = defaults & auth ?~ oauth1Auth consumerKey consumerSecret accTok accTokSec
  void $ postWith opts reqUrl ([] :: [FormParam])

lookupAccessToken :: Credential -> BS.ByteString
lookupAccessToken = fromJust . lookup "oauth_token" . unCredential

lookupAccessTokenSecret :: Credential -> BS.ByteString
lookupAccessTokenSecret = fromJust . lookup "oauth_token_secret" . unCredential

main :: IO ()
main = do
  Right config <- parseServerArgs
  env <- Env <$> TMap.newIO <*> TMap.newIO <*> TMap.newIO
  registerLoop env config `race_` server env config

server :: Env -> Config -> IO ()
server Env{..} cfg@Config{..} = run oauthCallbackPort $ serve @OAuthCallbackAPI Proxy body
  where
    body :: Server OAuthCallbackAPI
    body (Just (Token tok)) (Just (Token verifier)) = do
      (user, cred) <- liftIO $ atomically $ do
        user <- lookupTMap' tok  tokToUser
        cred <- lookupTMap' user tempCredDic
        return (user, cred)
      let cred' = injectVerifier verifier cred
      man <- liftIO $ newManager tlsManagerSettings
      accToks <- getTokenCredential (configToOAuth cfg) cred' man
      liftIO $ atomically $ TMap.insert accToks user accTokDic
      return "Ok!"
    body _ _ =
      throwError err401 { errBody = "Something went wrong..." }

lookupTMap' :: (Eq k, Hashable k) => k -> TMap.Map k b -> STM b
lookupTMap' key dic = TMap.lookup key dic >>= \case
  Nothing -> retry
  Just a -> return a

registerLoop :: Env -> Config -> IO ()
registerLoop dic config@Config{..} = do
  registerWebhook config
  mapM_ (registerUser dic config) accounts


