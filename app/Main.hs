{-# LANGUAGE OverloadedStrings #-}
module Main where

import Eirini.Route
import KubernetesExtras.Client
import Kubernetes.OpenAPI.Model

main :: IO ()
main = do
  let gkeConfig = "/Users/axeman/.kube/config"
      ibmConfig = "/Users/axeman/.bluemix/plugins/container-service/clusters/scale-test/kube-config-lon06-scale-test.yml"
  (mgr,cfg) <- kubeClient $ KubeConfigFile ibmConfig
  routes <- interpretKubeResponseInIO mgr cfg (getRoutes $ Namespace "eirini")
  print routes
