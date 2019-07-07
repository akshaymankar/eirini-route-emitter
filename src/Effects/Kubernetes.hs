{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
module Effects.Kubernetes
  ( Kube(..)
  , runKubeM
  , executeRequest
  , createNamespace
  , deleteNamespace
  , createStatefulSet
  , getStatefulSet
  , deleteStatefulSet
  , listPods
  )
where

import Control.Exception            (bracket)
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Control.Monad.Reader
import Data.Aeson
import Data.Function                ((&))
import Data.JsonStream.Parser
import Data.Monoid                  as M
import Data.Text
import GHC.Stack
import Katip
import Kubernetes.Client.Watch
import Kubernetes.OpenAPI.Client
import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Model     hiding (Namespace, Watch)
import Network.HTTP.Client          (Manager)
import System.IO                    (stdout)

import qualified Control.Applicative             as A
import qualified Data.ByteString.Streaming.Char8 as Q
import qualified Data.Proxy                      as P (Proxy (..))
import qualified Kubernetes.OpenAPI.API.AppsV1   as AppsV1
import qualified Kubernetes.OpenAPI.API.CoreV1   as CoreV1
import qualified Kubernetes.OpenAPI.Model        as K
import qualified Streaming.Prelude               as S

-- TODO: This is not scalable, extend open api generator to generate This file
data Kube r where
  ExecuteRequest    :: (Produces req accept, MimeUnrender accept res, MimeType contentType)
                    => KubernetesRequest req contentType res accept
                    -> Kube res
  CreateNamespace   :: Text -> Kube V1Namespace
  DeleteNamespace   :: Name -> Kube V1Namespace
  ListPods          :: K.Namespace -> Kube V1PodList
  CreateStatefulSet :: V1StatefulSet -> K.Namespace -> Kube V1StatefulSet
  GetStatefulSet    :: Name -> K.Namespace -> Kube V1StatefulSet
  DeleteStatefulSet :: Name -> K.Namespace -> Kube V1Status
  ListStatefulSets  :: K.Namespace -> Kube V1StatefulSetList
  Watch             :: (Produces req accept, MimeUnrender accept res, MimeType contentType, FromJSON res, HasOptionalParam req K.Watch)
                    => KubernetesRequest req contentType res accept
                    -> (forall m. forall r.S.Stream (S.Of [WatchEvent res]) m r -> m ())
                    -> Kube ()
  WatchStatefulSets :: K.Namespace
                    -> (forall m. forall r.S.Stream (S.Of [WatchEvent V1StatefulSetList]) m r -> m ())
                    -> Kube ()
makeEffect ''Kube

kubeWithConfig :: HasCallStack => Manager -> KubernetesClientConfig -> Kube r -> IO r
kubeWithConfig mgr cfg (ExecuteRequest req) = do
  dispatchMime mgr cfg req
    & (fmap mimeResult)
    >>= either (error . mimeError) pure
kubeWithConfig mgr cfg (CreateNamespace name) =
  let ns = makeNamespaceFromName name
      req = CoreV1.createNamespace (ContentType MimeJSON) (Accept MimeJSON) ns
  in kubeWithConfig mgr cfg (ExecuteRequest req)
kubeWithConfig mgr cfg (DeleteNamespace name) =
  let req = deleteNamespaceReimpl (ContentType MimeJSON) (Accept MimeJSON) name
  in kubeWithConfig mgr cfg (ExecuteRequest req)

kubeWithConfig mgr cfg (ListPods ns) =
  let req = CoreV1.listNamespacedPod (Accept MimeJSON) ns
  in kubeWithConfig mgr cfg (ExecuteRequest req)

kubeWithConfig mgr cfg (CreateStatefulSet ss ns) =
  let req = AppsV1.createNamespacedStatefulSet (ContentType MimeJSON) (Accept MimeJSON) ss ns
  in kubeWithConfig mgr cfg (ExecuteRequest req)
kubeWithConfig mgr cfg (GetStatefulSet name ns) =
  let req = AppsV1.readNamespacedStatefulSet (Accept MimeJSON) name ns
  in kubeWithConfig mgr cfg (ExecuteRequest req)
kubeWithConfig mgr cfg (DeleteStatefulSet name ns) =
  let req = AppsV1.deleteNamespacedStatefulSet (ContentType MimeJSON) (Accept MimeJSON) name ns
  in kubeWithConfig mgr cfg (ExecuteRequest req)

kubeWithConfig mgr cfg (Watch req f) =
  let withResponseBody body = streamParse value body & f
  in dispatchWatch mgr cfg req withResponseBody
kubeWithConfig mgr cfg (WatchStatefulSets ns f) =
  let req = AppsV1.listNamespacedStatefulSet (Accept MimeJSON) ns
  in kubeWithConfig mgr cfg (Watch req f)

runKubeM :: forall effs a. LastMember IO effs
         => Manager
         -> KubernetesClientConfig
         -> Eff (Kube ': effs) a
         -> Eff effs a
runKubeM mgr cfg = interpretM $ kubeWithConfig mgr cfg

-- This is a reimplementation because of this issue: https://github.com/kubernetes/kubernetes/issues/59501
-- Ideally this should expect Namespace or Status, but for now this works
-- TODO: Fix the above issue or reimplement this properly
deleteNamespaceReimpl
  :: (Consumes CoreV1.DeleteNamespace contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  name of the Namespace
  -> KubernetesRequest CoreV1.DeleteNamespace contentType V1Namespace accept
deleteNamespaceReimpl _  _ (Name name) =
  _mkRequest "DELETE" ["/api/v1/namespaces/",toPath name]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyBearerToken)

-- This is a reimplementation because of this issue: https://github.com/kubernetes/kubernetes/issues/59501
-- Ideally this should expect V1StatefulSet or Status, but for now this works
-- TODO: Fix the above issue or reimplement this properly
deleteNamespacedStatefulSetReimpl
  :: (Consumes AppsV1.DeleteNamespacedStatefulSet contentType)
  => ContentType contentType -- ^ request content-type ('MimeType')
  -> Accept accept -- ^ request accept ('MimeType')
  -> Name -- ^ "name" -  name of the StatefulSet
  -> K.Namespace -- ^ "namespace" -  object name and auth scope, such as for teams and projects
  -> KubernetesRequest AppsV1.DeleteNamespacedStatefulSet contentType V1StatefulSet accept
deleteNamespacedStatefulSetReimpl _  _ (Name name) (K.Namespace namespace) =
  _mkRequest "DELETE" ["/apis/apps/v1/namespaces/",toPath namespace,"/statefulsets/",toPath name]
    `_hasAuthType` (P.Proxy :: P.Proxy AuthApiKeyBearerToken)


makeNamespaceFromName name = mkV1Namespace{v1NamespaceMetadata = pure $ makeObjectMetaFromName name}
makeObjectMetaFromName name = mkV1ObjectMeta{v1ObjectMetaName = pure name}

-- | Parse the stream using the given parser.
streamParse ::
  FromJSON a =>
    Parser a
    -> Q.ByteString IO r
    -> S.Stream (S.Of [a]) IO r
streamParse parser byteStream = do
  byteStream & Q.lines & parseEvent parser

-- | Parse a single event from the stream.
parseEvent ::
  (FromJSON a, Monad m) =>
    Parser a
    -> S.Stream (Q.ByteString m) m r
    -> S.Stream (S.Of [a]) m r
parseEvent parser byteStream = S.map (parseByteString parser) (S.mapped Q.toStrict byteStream)
