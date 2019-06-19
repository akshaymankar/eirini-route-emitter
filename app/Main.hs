{-# LANGUAGE OverloadedStrings #-}
module Main where

import Eirini.Route
import KubernetesExtras.Client
import Kubernetes.OpenAPI.Model
import KubernetesExtras.Auth.OIDC
import Control.Concurrent.STM

import qualified Data.Map as Map

main :: IO ()
main = do
  let gkeConfig = "/Users/axeman/.kube/config"
      ibmConfig = "/Users/axeman/.bluemix/plugins/container-service/clusters/scale-test/kube-config-lon06-scale-test.yml"
  oidcCache <- atomically $ newTVar $ Map.fromList []
  (mgr,cfg) <- kubeClient oidcCache $ KubeConfigFile ibmConfig
  interpretKubeResponseInIO mgr cfg (getRoutes $ Namespace "eirini")
    >>= print
