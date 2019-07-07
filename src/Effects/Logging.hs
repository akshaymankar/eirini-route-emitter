{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE Rank2Types       #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeOperators    #-}
module Effects.Logging
  ( LogLevel(..)
  , Logger
  , log
  , logDebug
  , logToStdoutM
  )
where

import Control.Monad.Freer
import Control.Monad.Freer.TH
import Data.Time.Clock
import Data.Text
import Katip
import Prelude                hiding (log)

data LogLevel = Debug
              | Info
              | Warning
              | Error
              | Fatal
              deriving (Show, Eq)

data Logger x where
  Log :: LogLevel -> Text -> Logger ()

makeEffect ''Logger

printToStdout :: Logger a -> IO a
printToStdout (Log level l) = do
  time <- getCurrentTime
  putStrLn $ "[" <> show level <> "][" <> show time <> "] " <> unpack l

logToStdoutM :: forall effs a. LastMember IO effs
             => Eff (Logger ': effs) a
             -> Eff effs a
logToStdoutM = interpretM printToStdout

logDebug :: Member Logger m => Text -> Eff m ()
logDebug = log Debug
