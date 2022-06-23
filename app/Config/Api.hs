{-# LANGUAGE DeriveGeneric #-}

module Config.Api where

import           Data.Aeson      (FromJSON, ToJSON)
import qualified Data.Aeson      as Aeson
import           Data.Map        (Map)
import           Data.Scientific (Scientific)
import           Data.Text       (Text)
import           GHC.Generics    (Generic)

newtype ConfigVersion =
  ConfigVersion Int
  deriving (Generic, Show)

instance FromJSON ConfigVersion

instance ToJSON ConfigVersion

data ConfigItem
  = Boolean !Bool
  | Number !Scientific
  | String !Text
  deriving (Show)

instance FromJSON ConfigItem where
  parseJSON (Aeson.Bool b)   = pure (Boolean b)
  parseJSON (Aeson.Number n) = pure (Number n)
  parseJSON (Aeson.String s) = pure (String s)
  parseJSON v                = fail $ "Cannot parse value " ++ show v

instance ToJSON ConfigItem where
  toJSON (Boolean b) = Aeson.Bool b
  toJSON (Number n)  = Aeson.Number n
  toJSON (String s)  = Aeson.String s

data ConfigResponse =
  ConfigResponse
    { version :: !ConfigVersion
    , items   :: !(Map Text ConfigItem)
    }
  deriving (Generic, Show)

instance FromJSON ConfigResponse

instance ToJSON ConfigResponse
