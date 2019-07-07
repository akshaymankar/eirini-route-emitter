{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
module Effects.Nats
  ( NatsOperation(..)
  , interpretNatsOperationInIO
  , natsPublish
  , natsSubscribe
  , natsUnsubscribe
  , runNatsM
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Network.Nats.Client
import Network.Nats.Protocol

import qualified Data.ByteString as BS

data NatsOperation f where
  NatsPublish :: Subject -> BS.ByteString -> NatsOperation ()
  NatsSubscribe :: Subject -> MessageHandler -> Maybe QueueGroup -> NatsOperation SubscriptionId
  NatsUnsubscribe :: SubscriptionId -> Maybe Int -> NatsOperation ()
makeEffect ''NatsOperation

interpretNatsOperationInIO :: NatsClient -> Eff '[NatsOperation] a -> IO a
interpretNatsOperationInIO client = runM . translate (natsOperationToIO client)

natsOperationToIO :: NatsClient -> NatsOperation r -> IO r
natsOperationToIO natsClient (NatsPublish subject payload) = publish natsClient subject payload
natsOperationToIO natsClient (NatsSubscribe subject handler queueGroup) = subscribe natsClient subject handler queueGroup
natsOperationToIO natsClient (NatsUnsubscribe subId msgs) = unsubscribe natsClient subId msgs

runNatsM :: forall effs a. LastMember IO effs
         => NatsClient
         -> Eff (NatsOperation ': effs) a
         -> Eff effs a
runNatsM c = interpretM $ natsOperationToIO c
