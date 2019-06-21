{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
module FreerNats.Client
  ( NatsOperation(..)
  , interpretNatsOperationInIO
  , natsPublish
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

