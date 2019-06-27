{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Eirini.Route.Publish
  (publishRoutes)
where

import Control.Monad.Freer
import Data.Aeson              (encode)
import Data.OpenUnion.Internal
import Data.Text
import Effects.Logging
import Effects.Nats
import Eirini.Route.Types
import Network.Nats.Client

import qualified Data.ByteString.Lazy as LBS

publishRoutes :: (FindElem NatsOperation m, FindElem Logger m)
              => Subject -> [RouteMessage] -> Eff m ()
publishRoutes subject routes = do
  logDebug $ "publishing " <> pack (show $ Prelude.length routes) <> " routes"
  mapM_ (natsPublish subject . LBS.toStrict . encode) routes

