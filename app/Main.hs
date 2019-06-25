{-# LANGUAGE OverloadedStrings #-}
module Main where

import Eirini.Route.Collect
import Eirini.Route.Publish
import KubernetesExtras.Client
import Kubernetes.OpenAPI.Model
import KubernetesExtras.Auth.OIDC
import Control.Concurrent.STM
import Network.Nats.Client
import FreerKube.Response
import FreerNats.Client
import Control.Concurrent
import Control.Monad

import qualified Data.Map as Map

main :: IO ()
main = do
  let gkeConfig = "/Users/eirini/.kube/config"
      ibmConfig = "/Users/axeman/.bluemix/plugins/container-service/clusters/acceptance/kube-config-lon06-acceptance.yml"
  oidcCache <- atomically $ newTVar $ Map.fromList []
  (mgr,cfg) <- kubeClient oidcCache $ KubeConfigFile gkeConfig
  client <- connect defaultConnectionSettings 10
  subject <- either error pure $ createSubject "router.register"
  forever $ collectAndPublish mgr cfg client subject >> (threadDelay $ 30 * seconds)

collectAndPublish mgr cfg client subject = do
  routes <- interpretKubeResponseInIO mgr cfg (collectRoutes $ Namespace "eirini")
  interpretNatsOperationInIO client $ publishRoutes subject routes

seconds = 1000000
