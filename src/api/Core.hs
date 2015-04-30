{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Api.Core where

import           Control.Lens (makeLenses)
import           Snap.Core
import           Snap.Snaplet
import qualified Data.ByteString.Char8 as B

import Api.Services.Eve

data Api = Api { _applianceService :: Snaplet ApplianceService }
makeLenses ''Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200

apiInit :: SnapletInit b Api
apiInit = makeSnaplet "api" "Core Api" Nothing $ do
  ts <- nestSnaplet "appliances" applianceService applianceServiceInit
  addRoutes apiRoutes
  return $ Api ts
