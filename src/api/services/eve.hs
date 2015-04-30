{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Api.Services.Eve where

import           Api.Types
import           Control.Applicative
import           Control.Lens (makeLenses)
import           Control.Monad
import           Control.Monad.State.Class (get)
import           Data.Aeson
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.ByteString.Char8 as B
import qualified Data.Text as T

data ApplianceService = ApplianceService { _pg :: Snaplet Postgres }
makeLenses ''ApplianceService

applianceRoutes :: [(B.ByteString, Handler b ApplianceService ())]
applianceRoutes = [ ("/", method GET getAppliances)
                  , ("/", method PUT changeAppliance)
                  ]

getAppliances :: Handler b ApplianceService ()
getAppliances = do
  appliances <- query_ "SELECT * FROM appliances"
  modifyResponse $ setHeader "Content-Type" "application/json"
  writeLBS . encode $ (appliances :: [Appliance])

data Change = Change
  { changeId    :: Int
  , changeState :: T.Text
  }

instance FromJSON Change where
  parseJSON (Object v) = Change      <$>
                         v .: "id"   <*>
                         v .: "state"
  parseJSON _          = mzero

changeAppliance :: Handler b ApplianceService ()
changeAppliance = do
  (change :: Maybe Change) <- decode <$> readRequestBody 5000
  case change of
       Nothing                  -> writeLBS "failure"
       Just (Change id' state) -> do
         _ <- execute "UPDATE appliances SET state = (?) WHERE id = (?)" [state, T.pack $ show id']
         writeLBS "ok"

instance HasPostgres (Handler b ApplianceService) where
  getPostgresState = with pg get

applianceServiceInit :: SnapletInit b ApplianceService
applianceServiceInit = makeSnaplet "appliances" "Appliance Service" Nothing $ do
  pg' <- nestSnaplet "pg" pg pgsInit
  addRoutes applianceRoutes
  return $ ApplianceService pg'
