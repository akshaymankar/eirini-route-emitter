{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module Effects.Kubernetes
  ( KubeResponse
  , interpretKubeResponseInIO
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

data KubeResponse r where
  ExecuteRequest :: (Produces req accept, MimeUnrender accept res, MimeType contentType)
                 => KubernetesRequest req contentType res accept
                 -> KubeResponse res
makeEffect ''KubeResponse

interpretKubeResponseInIO :: Manager -> KubernetesClientConfig -> Eff '[KubeResponse] a -> IO a
interpretKubeResponseInIO mgr cfg = runM . translate (kubeResponseToIO mgr cfg)

kubeResponseToIO :: Manager -> KubernetesClientConfig -> KubeResponse r -> IO r
kubeResponseToIO mgr cfg (ExecuteRequest r) = dispatchMime mgr cfg r
                                              & (fmap mimeResult)
                                              >>= either (error . mimeError) pure

runKubeM :: forall effs a. LastMember IO effs
                 => Manager
                 -> KubernetesClientConfig
                 -> Eff (KubeResponse ': effs) a
                 -> Eff effs a
runKubeM mgr cfg = interpretM $ kubeResponseToIO mgr cfg
