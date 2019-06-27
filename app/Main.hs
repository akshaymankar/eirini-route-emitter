{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import Control.Monad.Freer
import Data.OpenUnion.Internal
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

import qualified Data.Map as Map

main :: IO ()
main = do
  let gkeConfig = "/Users/axeman/.kube/config"
      ibmConfig = "/Users/axeman/.bluemix/plugins/container-service/clusters/acceptance/kube-config-lon06-acceptance.yml"
  oidcCache <- atomically $ newTVar $ Map.fromList []
  (mgr,cfg) <- kubeClient oidcCache $ KubeConfigFile ibmConfig
  client <- connect defaultConnectionSettings 10
  subject <- either error pure $ createSubject "router.register"
  forever $ collectAndPublishIO mgr cfg client subject >> (threadDelay $ 30 * seconds)

collectAndPublishIO mgr cfg client subject = do
  runM
    . runKubeM mgr cfg
    . runNatsM client
    . logToStdoutM
    $ collectAndPublish subject

collectAndPublish :: ( FindElem Kube m
                     , FindElem NatsOperation m
                     , FindElem Logger m
                     )
                  => Subject -> Eff m ()
collectAndPublish subject = do
  routes <- collectRoutes $ Namespace "eirini"
  publishRoutes subject routes

seconds = 1000000
