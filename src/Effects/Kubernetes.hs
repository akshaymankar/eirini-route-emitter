{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Effects.Kubernetes
  ( Kube
  , executeRequest
  , runKubeM
  )
where

import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Core
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Network.HTTP.Client (Manager)
import Kubernetes.OpenAPI.Client
import Data.Function ((&))

data Kube r where
  ExecuteRequest :: (Produces req accept, MimeUnrender accept res, MimeType contentType)
                 => KubernetesRequest req contentType res accept
                 -> Kube res
makeEffect ''Kube

kubeWithConfig :: Manager -> KubernetesClientConfig -> Kube r -> IO r
kubeWithConfig mgr cfg (ExecuteRequest r) = dispatchMime mgr cfg r
                                              & (fmap mimeResult)
                                              >>= either (error . mimeError) pure

runKubeM :: forall effs a. LastMember IO effs
                 => Manager
                 -> KubernetesClientConfig
                 -> Eff (Kube ': effs) a
                 -> Eff effs a
runKubeM mgr cfg = interpretM $ kubeWithConfig mgr cfg
