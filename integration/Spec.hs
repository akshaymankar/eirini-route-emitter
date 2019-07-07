{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
import Control.Concurrent.STM
import Control.Monad.Freer
import Data.Text
import Effects.Kubernetes
import Effects.Nats
import Eirini.IntegrationSpec
import Kubernetes.OpenAPI.Model
import KubernetesExtras.Client
import Network.Nats.Client      as Nats
import Network.Socket
import Options.Applicative
import System.Environment
import Test.Hspec

import qualified Data.Map as Map

data TestOptions = TestOptions { kubeConfig :: Text
                               , natsHost   :: String
                               , natsPort   :: PortNumber
                               , namespace  :: Namespace
                               }
                   deriving (Show, Eq)

optParser :: Parser TestOptions
optParser = TestOptions
            <$> strOption (long "kubeconfig"
                           <> metavar "KUBECONFIG")
            <*> strOption (long "nats-host"
                           <> metavar "NATS-HOST")
            <*> option auto (long "nats-port"
                             <> metavar "NATS-port")
            <*> (Namespace
                 <$> strOption (long "namespace"
                                 <> short 'n'))

main :: IO ()
main = do
  let optsInfo = info
             (optParser <**> helper)
             (fullDesc
              <> progDesc "Run Integration tests for Eirini Route Emitter")
  TestOptions{..} <- execParser optsInfo
  oidcCache <- atomically $ newTVar $ Map.fromList []
  nats <- Nats.connect (ConnectionSettings natsHost natsPort) 10
  (mgr, cfg) <- kubeClient oidcCache $ KubeConfigFile $ unpack kubeConfig
  withArgs [] $ (hspec $ spec (runM . runKubeM mgr cfg) (runM . runNatsM nats) namespace)
