{-# LANGUAGE FlexibleContexts #-}
module Eirini.Route.Publish
  (publishRoutes)
where

import Control.Monad.Freer
import Network.Nats.Client
import FreerNats.Client
import Eirini.Route.Types
import Data.Aeson (encode)

import qualified Data.ByteString.Lazy as LBS

publishRoutes :: Member NatsOperation m => Subject -> [RouteMessage] -> Eff m ()
publishRoutes subject = mapM_ (natsPublish subject . LBS.toStrict . encode)

