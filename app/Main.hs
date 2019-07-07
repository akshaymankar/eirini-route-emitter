{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Freer
import Data.OpenUnion.Internal
import Data.Text
import Effects.Kubernetes
import Effects.Logging
import Effects.Nats
import Eirini.Route.Collect
import Eirini.Route.Publish
import Eirini.Route.Types
import Kubernetes.OpenAPI.Model
import KubernetesExtras.Auth.OIDC
import KubernetesExtras.Client
import Network.Nats.Client
import Network.Socket             (HostName, PortNumber)
import Options.Applicative

import qualified Data.Map as Map

data Options = Options { kubeConfigFile :: Maybe FilePath
                       , natsHost       :: HostName
                       , natsPort       :: PortNumber
                       , namespace      :: Namespace
                       }

optionsParser :: Parser Options
optionsParser = Options
                <$> option auto (long "kubeconfig"
                                 <> value Nothing
                                 <> metavar "PATH"
                                 <> help "location of kube config, leave empty to use in cluster config"
                                )
                <*> strOption (long "nats-host"
                               <> value "localhost"
                               <> metavar "HOST"
                               <> help "hostname for nats server"
                              )
                <*> option auto (long "nats-port"
                                 <> value 4222
                                 <> metavar "PORT"
                                 <> help "port number for nats server"
                                )
                <*> (Namespace <$> strOption (long "namespace"
                                              <> short 'n'
                                              <> value "eirini"
                                              <> metavar "NAMESPACE"
                                              <> help "namespace for eirini deployments"
                                             ))

main :: IO ()
main = do
  let opts = info (optionsParser <**> helper)
        ( fullDesc
          <> progDesc "Eirini Route Emitter")
  Options{..} <- execParser opts

  let kubeConfig = maybe (KubeConfigCluster) (KubeConfigFile) kubeConfigFile
  oidcCache <- atomically $ newTVar $ Map.fromList []
  (mgr,cfg) <- kubeClient oidcCache kubeConfig
  natsClient <- connect (ConnectionSettings natsHost natsPort) 10
  subject <- either error pure $ createSubject "router.register"
  every30seconds
    $ runEffects mgr cfg natsClient
    $ collectAndPublish namespace subject

runEffects mgr cfg natsClient =  runM
                             . runKubeM mgr cfg
                             . runNatsM natsClient
                             . logToStdoutM

collectAndPublish :: ( FindElem Kube m
                     , FindElem NatsOperation m
                     , FindElem Logger m
                     )
                  => Namespace -> Subject -> Eff m ()
collectAndPublish ns subject = collectRoutes ns
                               >>= publishRoutes subject


every30seconds :: IO a -> IO ()
every30seconds i = forever $ i >> (threadDelay $ 30 * seconds)
seconds = 1_000_000
