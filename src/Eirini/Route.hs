{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
module Eirini.Route where

import Control.Error.Safe            (rightZ)
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Data.Aeson
import Data.Aeson.Casing
import Data.Aeson.TH
import Data.Function                 ((&))
import Data.Maybe
import Data.Text                     hiding (concat, map)
import Data.Text.Encoding
import Data.Word
import Kubernetes.OpenAPI.API.AppsV1
import Kubernetes.OpenAPI.API.CoreV1
import Kubernetes.OpenAPI.Client
import Kubernetes.OpenAPI.Core
import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Model
import Network.HTTP.Client           (Manager)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map             as Map

data RouteMessage = RouteMessage { registeredRoutes :: [Text]
                                 , unregistedRoutes :: [Text]
                                 , instanceID       :: Text
                                 , name             :: Text
                                 , address          :: Text
                                 , port             :: Word32
                                 }
  deriving (Show, Eq)

data RouteAnnotation = RouteAnnotation { raHostname :: Text, raPort :: Word32}
  deriving (Show, Eq)

$(deriveJSON (aesonPrefix snakeCase) ''RouteAnnotation)

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
                                              >>= mimeResultToIO

mimeResultToIO :: Either MimeError res -> IO res
mimeResultToIO (Left MimeError{mimeError=e}) = error e
mimeResultToIO (Right res)                   = pure res

getRoutes :: Member KubeResponse m => Namespace -> Eff m [RouteMessage]
getRoutes ns = do
  pods <- v1PodListItems <$> (executeRequest $ listNamespacedPod (Accept MimeJSON) ns)
  statefulsets <- v1StatefulSetListItems <$> (executeRequest $ listNamespacedStatefulSet (Accept MimeJSON) ns)
  return $ concat $ catMaybes $ map (extractRoute statefulsets) pods

extractRoute ::  [V1StatefulSet] -> V1Pod -> Maybe [RouteMessage]
extractRoute statefulsets pod = do
  owners <- v1ObjectMetaOwnerReferences =<< v1PodMetadata pod
  ssName <- v1OwnerReferenceName <$> (listToMaybe $ Prelude.filter (\o -> v1OwnerReferenceKind o == "StatefulSet") owners)
  statefulset <- listToMaybe $ Prelude.filter (\s -> (v1ObjectMetaName =<< v1StatefulSetMetadata s) == Just ssName) statefulsets
  ssMetadata <- v1StatefulSetMetadata statefulset
  routeAnnotationsStr <- Map.lookup "routes" =<< v1ObjectMetaAnnotations ssMetadata
  routes <- decode $ LBS.fromStrict $ encodeUtf8 routeAnnotationsStr :: Maybe [RouteAnnotation]
  sequence $ Prelude.map (mkRouteMessage pod) routes

mkRouteMessage :: V1Pod -> RouteAnnotation -> Maybe RouteMessage
mkRouteMessage pod (RouteAnnotation host port) = do
  instanceID <- v1ObjectMetaName =<< v1PodMetadata pod
  address <- v1PodStatusPodIp =<< v1PodStatus pod
  let registeredRoutes = [host]
      unregistedRoutes = []
      name = instanceID
  return RouteMessage{..}
