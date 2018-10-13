{-# LANGUAGE DataKinds, DeriveGeneric, DerivingStrategies, FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, KindSignatures                     #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, RecordWildCards         #-}
module Relayeet.Activity
  ( Activity(..), Favorite(..), Follow(..), Block(..), Mute(..)
  , UserEvent(..), AppID(..), UserID(..), App(..), DirectMessageEvent(..)
  , SimpleUser(..), simpleUser, Singleton(..), DirectMessageData(..)
  , MessageCreate(..)
  , module Web.Twitter.Types) where
import Web.Twitter.Types

import           Control.Monad       (when)
import           Data.Aeson          (FromJSON (..), GFromJSON, Options,
                                      SumEncoding (..), ToJSON (..), Value,
                                      Zero, camelTo2, constructorTagModifier,
                                      defaultOptions, fieldLabelModifier,
                                      genericParseJSON, genericToJSON, object,
                                      sumEncoding, tagSingleConstructors,
                                      unwrapUnaryRecords, (.:), (.=))
import           Data.Aeson.Types    (Parser, withObject)
import           Data.HashMap.Strict (HashMap)
import           Data.Proxy          (Proxy (..))
import           Data.String         (IsString)
import           Data.Text           (Text)
import qualified Data.Text           as T
import           GHC.Generics        (Generic, Rep)
import           GHC.TypeLits        (KnownSymbol, Symbol, symbolVal)
import           Data.Time.Format    (formatTime, defaultTimeLocale)

data SimpleUser = SimpleUser { usrScreenName           :: Text
                             , usrProfileImageUrl      :: Maybe Text
                             , usrProtected            :: Bool
                             , usrLocation             :: Maybe Text
                             , usrUrl                  :: Maybe Text
                             , usrProfileImageUrlHttps :: Maybe Text
                             , usrVerified             :: Bool
                             , usrStatusesCount        :: Integer
                             , usrCreatedTimestamp     :: Text
                             , usrName                 :: Text
                             , usrId                   :: Text
                             , usrFriendsCount         :: Integer
                             , usrDescription          :: Maybe Text
                             , usrFollowersCount       :: Integer
                             }
  deriving (Show, Eq, Generic)

simpleUser :: User -> SimpleUser
simpleUser User{..} =
  SimpleUser{ usrScreenName = userScreenName
            , usrProfileImageUrl = userProfileImageURL
            , usrProfileImageUrlHttps = userProfileImageURLHttps 
            , usrProtected = userProtected
            , usrLocation = userLocation
            , usrUrl = userURL
            , usrVerified = userVerified
            , usrStatusesCount = fromIntegral userStatusesCount
            , usrCreatedTimestamp = T.pack $ formatTime defaultTimeLocale "%c" userCreatedAt 
            , usrName =  userScreenName
            , usrId = T.pack $ show userId
            , usrFriendsCount = fromIntegral userFriendsCount
            , usrDescription  = userDescription
            , usrFollowersCount = fromIntegral userFollowersCount
            }

instance ToJSON SimpleUser where
  toJSON = genericToJSON appOpts

instance FromJSON SimpleUser where
  parseJSON = genericParseJSON appOpts

data Activity = TweetCreateEvents { forUserId         :: Text
                                  , tweetCreateEvents :: [Status] }
              | FavoriteEvents { favoriteEvents :: [Favorite] }
              | FollowEvents { forUserId    :: Text
                             , followEvents :: [Follow]
                             }
              | BlockEvents { forUserId   :: Text
                            , blockEvents :: [Block]
                            }
              | MuteEvents { forUserId :: Text, muteEvents :: [Mute] }
              | UserEvent { userEvent :: UserEvent }
              | DirectMessageEvents { forUserId :: Text
                                    , directMessageEvents :: [DirectMessageEvent]
                                    , apps :: Maybe (HashMap Text App)
                                    , users :: Maybe (HashMap Text SimpleUser)
                                    }
              deriving (Show, Eq, Generic)

data App = App { appUrl  :: Text
               , appName :: Text
               , appId   :: Text
               }
  deriving (Show, Eq, Generic)

appOpts :: Options
appOpts = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3
                         }

instance ToJSON App where
  toJSON = genericToJSON appOpts

instance FromJSON App where
  parseJSON = genericParseJSON appOpts

newtype Singleton (lab :: Symbol) a = Singleton { runSingleton :: a }
  deriving (Read, Show, Eq, Ord, Generic)
  deriving newtype (IsString)

instance (KnownSymbol lab, ToJSON a) => ToJSON (Singleton lab a) where
  toJSON (Singleton a) = object [ T.pack (symbolVal (Proxy @lab)) .= a ]

instance (KnownSymbol lab, FromJSON a) => FromJSON (Singleton lab a) where
  parseJSON =
    let lab = T.pack (symbolVal (Proxy @lab))
    in withObject ("Singleton object with label " ++ show lab) $ \obj -> do
      when (length obj /= 1) $  fail "Object is not singleton!"
      Singleton <$> obj .: lab

data DirectMessageEvent = DME { dmeId               :: Text
                              , dmeCreatedTimestamp :: Text
                              , dmeMessageCreate    :: MessageCreate
                              }
  deriving (Show, Eq, Generic)

data MessageCreate = MessageCreate { mcTarget   :: Singleton "recipient_id" Text
                                   , mcSenderId :: Text
                                   , mcSourceAppId :: Text
                                   , mcMessageData :: DirectMessageData
                                   }
  deriving (Show, Eq, Generic)

data DirectMessageData = DMData { dmdText     :: Text
                                , dmdEntities :: Entities
                                }
  deriving (Show, Eq, Generic)


dmDataOpts :: Options
dmDataOpts = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3
                            , sumEncoding = UntaggedValue
                            , constructorTagModifier = camelTo2 '_'
                            }

instance FromJSON DirectMessageData where
  parseJSON = genericParseJSON dmDataOpts

instance ToJSON DirectMessageData where
  toJSON = genericToJSON dmDataOpts

dmeOpts :: Options
dmeOpts = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop 3
                         , sumEncoding = TaggedObject "type" ""
                         , constructorTagModifier = camelTo2 '_'
                         }

instance FromJSON DirectMessageEvent where
  parseJSON = genericParseJSON dmeOpts

instance ToJSON DirectMessageEvent where
  toJSON = genericToJSON dmeOpts

dmOpts :: Options
dmOpts = dmeOpts { fieldLabelModifier = camelTo2 '_' . drop 2 }

instance FromJSON MessageCreate where
  parseJSON = genericParseJSON dmOpts

instance ToJSON MessageCreate where
  toJSON = genericToJSON dmOpts

actOpts :: Options
actOpts = defaultOptions { fieldLabelModifier = camelTo2 '_'
                         , sumEncoding = UntaggedValue
                         }

instance FromJSON Activity where
  parseJSON = genericParseJSON actOpts

instance ToJSON Activity where
  toJSON = genericToJSON actOpts

data Favorite = Favorite { favId              :: Text
                         , favCreatedAt       :: Text
                         , favTimestampMs     :: Integer
                         , favFavoritedStatus :: Status
                         , favUser            :: User
                         }
  deriving (Show, Eq, Generic)

instance FromJSON Favorite where
  parseJSON = parseCamelDrop 3

instance ToJSON Favorite where
  toJSON = genericToJSON $ camelDrop 3

data Follow = Follow { followType             :: Text
                     , followCreatedTimestamp :: Text
                     , followTarget           :: User
                     , followSource           :: User
                     }
  deriving (Show, Eq, Generic)

instance FromJSON Follow where
  parseJSON = parseCamelDrop 6

instance ToJSON Follow where
  toJSON = genericToJSON $ camelDrop 6

data Block = Block { blockType             :: Text
                   , blockCreatedTimestamp :: Text
                   , blockTarget           :: User
                   , blockSource           :: User
                   }
  deriving (Show, Eq, Generic)

instance FromJSON Block where
  parseJSON = parseCamelDrop 5

instance ToJSON Block where
  toJSON = genericToJSON $ camelDrop 5

data Mute = Mute { muteType             :: Text
                 , muteCreatedTimestamp :: Text
                 , muteTarget           :: User
                 , muteSource           :: User
                 }
  deriving (Show, Eq, Generic)

instance FromJSON Mute where
  parseJSON = parseCamelDrop 4

instance ToJSON Mute where
  toJSON = genericToJSON $ camelDrop 4

camelDrop :: Int -> Options
camelDrop n = defaultOptions { fieldLabelModifier = camelTo2 '_' . drop n
                             }

parseCamelDrop :: (Generic a, GFromJSON Zero (Rep a)) => Int -> Value -> Parser a
parseCamelDrop = genericParseJSON . camelDrop


data UserEvent = Revoke { ueDateTime :: Text
                        , ueTarget   :: AppID
                        , ueSource   :: UserID
                        }
  deriving (Read, Show, Eq, Ord, Generic)

ueOpts :: Options
ueOpts = (camelDrop 2) { sumEncoding = ObjectWithSingleField
                       , constructorTagModifier = camelTo2 '_'
                       , tagSingleConstructors  = True
                       , unwrapUnaryRecords = True
                       }

instance FromJSON UserEvent where
  parseJSON = genericParseJSON ueOpts

instance ToJSON UserEvent where
  toJSON = genericToJSON ueOpts

newtype AppID  = AppID { appID :: Text }
  deriving (Read, Show, Eq, Ord, Generic)
newtype UserID  = UserID { ruserID :: Text }
  deriving (Read, Show, Eq, Ord, Generic)

idsOpts :: Options
idsOpts = defaultOptions { sumEncoding = ObjectWithSingleField
                         , constructorTagModifier = camelTo2 '_'
                         , tagSingleConstructors  = True
                         , unwrapUnaryRecords = True
                         }

instance FromJSON AppID where
  parseJSON = genericParseJSON idsOpts

instance ToJSON AppID where
  toJSON = genericToJSON idsOpts

instance FromJSON UserID where
  parseJSON = genericParseJSON idsOpts

instance ToJSON UserID where
  toJSON = genericToJSON idsOpts
