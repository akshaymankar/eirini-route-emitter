{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE GADTs             #-}
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
import Effects.Kubernetes
import Effects.Logging
import Eirini.Route.Types
import Kubernetes.OpenAPI.API.AppsV1
import Kubernetes.OpenAPI.API.CoreV1
import Kubernetes.OpenAPI.MimeTypes
import Kubernetes.OpenAPI.Model

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map             as Map
import qualified Data.Text            as Text

collectRoutes :: (FindElem Kube m, FindElem Logger m)
              => Namespace
              -> Eff m [RouteMessage]
collectRoutes ns = do
  _ <- logDebug "starting to collect routes"
  pods <- v1PodListItems <$>listPods ns
  statefulsets <- v1StatefulSetListItems <$> listStatefulSets ns
  let routes = concat $ catMaybes $ map (extractRoute statefulsets) pods
  _ <- logDebug $ "collected: " <> (Text.pack $ show $ length routes) <> " routes"
  return routes

extractRoute :: [V1StatefulSet] -> V1Pod -> Maybe [RouteMessage]
extractRoute statefulsets pod = do
  owners <- v1ObjectMetaOwnerReferences =<< v1PodMetadata pod
  ssName <- v1OwnerReferenceName <$> (listToMaybe $ Prelude.filter (\o -> v1OwnerReferenceKind o == "StatefulSet") owners)
  statefulset <- listToMaybe $ Prelude.filter (\s -> (v1ObjectMetaName =<< v1StatefulSetMetadata s) == Just ssName) statefulsets
  ssMetadata <- v1StatefulSetMetadata statefulset
  routeAnnotationsStr <- Map.lookup "routes" =<< v1ObjectMetaAnnotations ssMetadata
  routes <- decode $ LBS.fromStrict $ encodeUtf8 routeAnnotationsStr :: Maybe [RouteAnnotation]
  sequence $ Prelude.map (mkRouteMessage pod) routes

mkRouteMessage :: V1Pod -> RouteAnnotation -> Maybe RouteMessage
mkRouteMessage pod (RouteAnnotation uri port) = do
  privateInstanceId <- v1ObjectMetaName =<< v1PodMetadata pod
  host <- v1PodStatusPodIp =<< v1PodStatus pod
  let uris = [uri]
      app = privateInstanceId
  return RouteMessage{..}
