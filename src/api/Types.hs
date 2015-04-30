{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Types where

import           Control.Applicative
import qualified Data.Text as T
import           Data.Aeson (ToJSON(toJSON), object, (.=))
import           Snap.Snaplet.PostgresqlSimple

data Appliance = Appliance
  { applianceId     :: Int
  , applianceState  :: T.Text
  } deriving Show

instance FromRow Appliance where
  fromRow = Appliance <$> field
                      <*> field

instance ToJSON Appliance where
   toJSON (Appliance id' state) = object [ "id" .= id', "state" .= state ]
