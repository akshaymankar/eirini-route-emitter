{-# LANGUAGE OverloadedStrings #-}
module Main where

import Eirini.Route.Collect
import Eirini.Route.Publish
import KubernetesExtras.Client
import Kubernetes.OpenAPI.Model
import Kubernetes.OpenAPI.Core
import KubernetesExtras.Auth.OIDC
import Control.Concurrent.STM
import Network.Nats.Client
import FreerKube.Response
import FreerNats.Client
import Network.HTTP.Client
import Control.Monad
import Control.Concurrent

import qualified Data.Map as Map

main :: IO ()
main = do
  let gkeConfig = "/Users/axeman/.kube/config"
      ibmConfig = "/Users/eirini/.bluemix/plugins/container-service/clusters/acceptance/kube-config-lon06-acceptance.yml"
  oidcCache <- atomically $ newTVar $ Map.fromList []
  (mgr,cfg) <- kubeClient oidcCache $ KubeConfigFile ibmConfig
  natsClient <- connect defaultConnectionSettings 10
  subject <- either error pure $ createSubject "router.register"
  forever $ collectAndPublishRoutes mgr cfg natsClient subject -- >> print "sleeping" >> threadDelay 30000000

collectAndPublishRoutes :: Manager -> KubernetesClientConfig -> NatsClient -> Subject -> IO ()
collectAndPublishRoutes mgr cfg natsClient subject = do
  routes <- interpretKubeResponseInIO mgr cfg (collectRoutes $ Namespace "eirini")
  interpretNatsOperationInIO natsClient $ publishRoutes subject routes
