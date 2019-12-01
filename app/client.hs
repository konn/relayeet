{-# LANGUAGE ExtendedDefaultRules, NamedFieldPuns, OverloadedStrings #-}
{-# LANGUAGE RecordWildCards, TypeApplications                       #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main where
import Relayeet

import           Conduit
import           Control.Concurrent
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception
import           Control.Monad
import           Control.Monad.Loops
import           Data.Aeson
import qualified Data.ByteString          as BS
import qualified Data.HashMap.Strict      as HM
import           Data.Maybe
import qualified Data.Text                as T
import qualified Data.Text.Lazy           as LT
import           Data.Text.Lazy.Builder
import           HTMLEntities.Decoder
import           Network.HTTP.Conduit
import           Network.HTTP.Types       (hAuthorization)
import           Shelly

default (T.Text, Int)

main :: IO ()
main = do
  Right cfg@ClientConfig{..} <- parseClientArgs
  updated <- newTVarIO True
  forever $ bracket (do atomically $ writeTVar updated True ;async $ notifyLoop cfg updated) cancel $ \_ -> do
    whileM_ (atomically $ swapTVar updated False) $ threadDelay (15 * 10^6)
    putStrLn "No input after 15secs. Reconnecting..."
    threadDelay (10^6)

notifyLoop :: ClientConfig -> TVar Bool -> IO ()
notifyLoop cfg@ClientConfig{..} upd = do
  let auth = (hAuthorization, "Bearer " <> getBearer bearer)
      Just req0 = parseRequest url
      req = req0 { requestHeaders = auth : requestHeaders req0 }
  man <- newManager tlsManagerSettings
  runResourceT $ do
    src <- responseBody <$> http req man
    runConduit $ src .| linesUnboundedAsciiC
                     .| mapMC (\a -> do liftIO $ atomically $ writeTVar upd True; return a)
                     .| filterC (not . BS.null)
                     .| concatMapC (decodeStrict' @Activity)
                     .| concatMapC (filterEvents cfg)
                     .| filterC (unmuted mute)
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
                 | DMed { dmedFrom :: SimpleUser
                        , dmedTo   :: T.Text
                        , dmedText :: T.Text
                        }
                 | Liked Favorite
                 | Followed { follwedFrom :: User, followedTo :: User }
                 deriving (Show, Eq)

notifySender :: NotifyEvent -> SimpleUser
notifySender (RT u _)             = simpleUser u
notifySender (Mentioned u _ _)    = simpleUser u
notifySender (DMed u _ _)         = u
notifySender (Liked Favorite{..}) = simpleUser favUser
notifySender (Followed f _)       = simpleUser f

notifyText :: NotifyEvent -> T.Text
notifyText (Mentioned _ _ Status{..}) =  statusText
notifyText _                          = ""

unmuted :: MuteSettings -> NotifyEvent -> Bool
unmuted MuteSettings{..} evt =
  usrScreenName (notifySender evt) `notElem` muteUsers
  && all (`T.isInfixOf` notifyText evt) muteKeywords

defTimeout :: Maybe Int
defTimeout = Just 5

decodeEntities :: T.Text -> T.Text
decodeEntities = LT.toStrict . toLazyText . htmlEncodedText

renderNotify :: NotifyEvent -> Notification
renderNotify (RT u s@Status{..}) =
  let appIconImage = getIcon u
      title = mconcat ["RT'ed by @", userScreenName u]
      message = decodeEntities statusText
      onSuccess = Just $ tweetUrl s
      subtitle = Just $ "@" <> userScreenName statusUser
      timeout = defTimeout
  in Notification{..}
renderNotify (Mentioned src _ s@Status{..}) =
  let appIconImage = getIcon src
      title = mconcat [ "@", userScreenName src, " mentioned"]
      message = decodeEntities statusText
      timeout = Nothing
      subtitle = Nothing
      onSuccess = Just $ tweetUrl s
  in Notification{..}
renderNotify (DMed src _ text) =
  let appIconImage = getSimpleIcon src
      title = mconcat [ "@", usrScreenName src, " mentioned"]
      message = decodeEntities text
      timeout = Nothing
      subtitle = Nothing
      onSuccess = Just $ "https://twitter.com/direct_messages/create/" <> usrScreenName src
  in Notification{..}
renderNotify (Liked Favorite{..}) =
  let appIconImage = getIcon favUser
      title = mconcat [ "Liked by @", userScreenName favUser ]
      subtitle = Just $ userScreenName $ statusUser favFavoritedStatus
      message = decodeEntities $ statusText favFavoritedStatus
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

getSimpleIcon :: SimpleUser -> URIString
getSimpleIcon = fromMaybe defaultIcon . usrProfileImageUrl

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

relevantDMs :: ClientConfig -> HM.HashMap T.Text App -> HM.HashMap T.Text SimpleUser
            -> DirectMessageEvent -> Maybe NotifyEvent
relevantDMs ClientConfig{..} _apps usrs DME{..} = do
  let MessageCreate{..} = dmeMessageCreate
  dmedFrom <- HM.lookup mcSenderId usrs
  dmedTo <- usrScreenName <$> HM.lookup (runSingleton mcTarget) usrs
  guard $ dmedTo `elem` targets
  let dmedText = dmdText mcMessageData
  return DMed{..}

filterEvents :: ClientConfig -> Activity -> [NotifyEvent]
filterEvents cfg TweetCreateEvents{tweetCreateEvents = stats} =
  mapMaybe (relevantStatus cfg) stats
filterEvents cfg FavoriteEvents{favoriteEvents = favs} =
  mapMaybe (relevantFavs cfg) favs
filterEvents cfg FollowEvents{followEvents = fs} =
  mapMaybe (relevantFollows cfg) fs
filterEvents cfg DirectMessageEvents{directMessageEvents = evs, apps, users} =
  mapMaybe (relevantDMs cfg (fromMaybe HM.empty apps) (fromMaybe HM.empty users)) evs
filterEvents _ _ = []
