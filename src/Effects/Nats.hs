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
  , runNatsM
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Network.Nats.Client

import qualified Data.ByteString as BS

data NatsOperation f where
  NatsPublish :: Subject -> BS.ByteString -> NatsOperation ()
makeEffect ''NatsOperation

interpretNatsOperationInIO :: NatsClient -> Eff '[NatsOperation] a -> IO a
interpretNatsOperationInIO client = runM . translate (natsOperationToIO client)

natsOperationToIO :: NatsClient -> NatsOperation r -> IO r
natsOperationToIO natsClient (NatsPublish subject payload) = publish natsClient subject payload

runNatsM :: forall effs a. LastMember IO effs
         => NatsClient
         -> Eff (NatsOperation ': effs) a
         -> Eff effs a
runNatsM c = interpretM $ natsOperationToIO c
