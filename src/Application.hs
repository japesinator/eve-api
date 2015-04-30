{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Control.Lens
import Snap.Snaplet

import Api.Core

------------------------------------------------------------------------------
data App = App
    { _api   :: Snaplet Api }

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App
