{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Eirini.Route.Collect
  (collectRoutes)
where

import Control.Monad.Freer
import Data.Aeson                    (decode)
import Data.Maybe
import Data.OpenUnion.Internal
import Data.Text.Encoding            (encodeUtf8)
import Eirini.Route.Types
import FreerKube.Response
import Kubernetes.OpenAPI.API.AppsV1
import Kubernetes.OpenAPI.API.CoreV1
import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Model

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map             as Map

collectRoutes :: FindElem KubeResponse m => Namespace -> Eff m [RouteMessage]
collectRoutes ns = do
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
  privateInstanceId <- v1ObjectMetaName =<< v1PodMetadata pod
  host <- v1PodStatusPodIp =<< v1PodStatus pod
  let uris = [host]
      app = privateInstanceId
  return RouteMessage{..}

