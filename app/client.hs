{-# LANGUAGE ExtendedDefaultRules, NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, TypeApplications                       #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where
import Relayeet

import           Conduit
import           Control.Concurrent
import           Control.Monad
import           Data.Aeson
import qualified Data.ByteString      as BS
import           Data.Maybe
import qualified Data.Text            as T
import           Network.HTTP.Conduit
import           Network.HTTP.Types   (hAuthorization)
import           Shelly

default (T.Text)

main :: IO ()
main = do
  Right cfg@ClientConfig{..} <- parseClientArgs
  let auth = (hAuthorization, "Bearer " <> getBearer bearer)
      Just req0 = parseRequest url
      req = req0 { requestHeaders = auth : requestHeaders req0 }
  man <- newManager tlsManagerSettings
  runResourceT $ do
    src <- responseBody <$> http req man
    runConduit $ src .| linesUnboundedAsciiC .| filterC (not . BS.null)
                     .| concatMapC (decodeStrict' @Activity)
                     .| concatMapC (filterEvents cfg)
                     .| mapC renderNotify
                     .| mapM_C (liftIO . void . forkIO . notify)

data Notification = Notification { appIconImage :: T.Text
                                 , title        :: T.Text
                                 , timeout      :: Maybe Int
                                 , message      :: T.Text
                                 , subtitle     :: Maybe T.Text
                                 , onSuccess    :: Maybe T.Text
                                 }
                  deriving (Read, Show, Eq, Ord)


data NotifyEvent = RT User Status
                 | Mentioned { mentionedFrom :: User
                             , mentionedTo   :: T.Text
                             , mentionStatus :: Status }
                 | Liked Favorite
                 | Followed { follwedFrom :: User, followedTo :: User }
                 deriving (Show, Eq)

defTimeout :: Maybe Int
defTimeout = Just 5

renderNotify :: NotifyEvent -> Notification
renderNotify (RT u s@Status{..}) =
  let appIconImage = getIcon u
      title = mconcat ["RT'ed by @", userScreenName u]
      message = statusText
      onSuccess = Just $ tweetUrl s
      subtitle = Just $ "@" <> userScreenName statusUser
      timeout = defTimeout
  in Notification{..}
renderNotify (Mentioned src _ s@Status{..}) =
  let appIconImage = getIcon src
      title = mconcat [ "@", userScreenName src, " mentioned"]
      message = statusText
      timeout = Nothing
      subtitle = Nothing
      onSuccess = Just $ tweetUrl s
  in Notification{..}
renderNotify (Liked Favorite{..}) =
  let appIconImage = getIcon favUser
      title = mconcat [ "Liked by @", userScreenName favUser ]
      subtitle = Just $ userScreenName $ statusUser favFavoritedStatus
      message = statusText favFavoritedStatus
      timeout = defTimeout
      onSuccess = Just $ tweetUrl favFavoritedStatus
  in Notification{..}
renderNotify (Followed src targ) =
  let appIconImage = getIcon src
      title = mconcat [ "@", userScreenName src ," followed @"
                      , userScreenName targ
                      ]
      message = ""
      timeout = defTimeout
      onSuccess = Just $ "https://twitter.com/" <> userScreenName src
  in Notification{subtitle = Nothing, ..}

tweetUrl :: Status -> T.Text
tweetUrl Status{..} = mconcat
  ["https://twitter.com/", userScreenName statusUser, "/status/", T.pack $ show statusId]

getIcon :: User -> URIString
getIcon = fromMaybe defaultIcon . userProfileImageURL

defaultIcon :: URIString
defaultIcon = "https://abs.twimg.com/sticky/default_profile_images/default_profile.png"

notify :: Notification -> IO ()
notify Notification{..} = shelly $ do
  resl <- T.strip <$> cmd "alerter" "-appIcon" appIconImage
      "-title"   title
      "-message" message
      (concat [ ["-timeout", T.pack $ show limit] | limit <- maybeToList timeout ])
      (concat [ ["-subtitle", subt] | subt <- maybeToList subtitle ])
  unless (resl == "@TIMEOUT" || resl == "@CLOSED") $ mapM_ (cmd "open") onSuccess

relevantStatus :: ClientConfig -> Status -> Maybe NotifyEvent
relevantStatus ClientConfig{..} orig =
  case statusRetweetedStatus orig of
    Just rt'ed -> do
      guard $ userScreenName (statusUser rt'ed) `elem` targets
      return $ RT (statusUser orig) rt'ed
    Nothing -> do
      let Status{..} = orig
      whom <- statusInReplyToScreenName
      let mentioned = whom `elem` targets && userScreenName statusUser /= whom
      guard mentioned
      return $ Mentioned statusUser whom orig

relevantFavs :: ClientConfig -> Favorite -> Maybe NotifyEvent
relevantFavs ClientConfig{..} fav@Favorite{..} = do
  let Status{..} = favFavoritedStatus
  guard $ userScreenName statusUser `elem` targets
  return $ Liked fav

relevantFollows :: ClientConfig -> Follow -> Maybe NotifyEvent
relevantFollows ClientConfig{..} Follow{..} = do
  guard $ userName followTarget `elem` targets
  return $ Followed followSource followTarget

filterEvents :: ClientConfig -> Activity -> [NotifyEvent]
filterEvents cfg TweetCreateEvents{tweetCreateEvents = stats} =
  mapMaybe (relevantStatus cfg) stats
filterEvents cfg FavoriteEvents{favoriteEvents = favs} =
  mapMaybe (relevantFavs cfg) favs
filterEvents cfg FollowEvents{followEvents = fs} =
  mapMaybe (relevantFollows cfg) fs
filterEvents _ _ = []
