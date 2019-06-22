{-# LANGUAGE FlexibleContexts #-}
module Eirini.Route.Publish
  (publishRoutes)
where

import Control.Monad.Freer
import Data.Aeson              (encode)
import Data.OpenUnion.Internal
import Eirini.Route.Types
import FreerNats.Client
import Network.Nats.Client

import qualified Data.ByteString.Lazy as LBS

publishRoutes :: FindElem NatsOperation m => Subject -> [RouteMessage] -> Eff m ()
publishRoutes subject = mapM_ (natsPublish subject . LBS.toStrict . encode)

