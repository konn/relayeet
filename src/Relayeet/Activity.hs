{-# LANGUAGE DeriveGeneric, FlexibleContexts #-}
module Relayeet.Activity
  ( Activity(..), Favorite(..), Follow(..), Block(..), Mute(..)
  , UserEvent(..), AppID(..), UserID(..)
  , module Web.Twitter.Types) where
import Data.Aeson        (FromJSON (..), GFromJSON, Options, SumEncoding (..),
                          ToJSON (..), Value, Zero, camelTo2,
                          constructorTagModifier, defaultOptions,
                          fieldLabelModifier, genericParseJSON, genericToJSON,
                          sumEncoding, tagSingleConstructors,
                          unwrapUnaryRecords)
import Data.Aeson.Types  (Parser)
import Data.Text         (Text)
import GHC.Generics      (Generic, Rep)
import Web.Twitter.Types

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
              -- | DirectMessageEvents { forUserId :: Text
              --                       , directMessageEvents :: [DirectMessageEvent]
              --                       }
              -- | DirectMessageIndicateTypingEvents
              -- | DirectMessageMarkReadEvents
              -- | TweetDeleteEvents
  deriving (Show, Eq, Generic)

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
