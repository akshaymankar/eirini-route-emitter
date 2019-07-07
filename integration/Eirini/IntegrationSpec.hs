{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
module Eirini.IntegrationSpec where

import Control.Monad
import Control.Monad.Freer
import Control.Monad.Freer.TH
import Data.Function                ((&))
import Data.List                    (find)
import Data.OpenUnion.Internal
import Effects.Kubernetes
import Effects.Nats
import Kubernetes.Client.Watch
import Kubernetes.OpenAPI.Model
import Kubernetes.OpenAPI.ModelLens
import Lens.Micro
import Network.Nats.Client
import Test.Hspec
import Control.Concurrent.MVar
import System.Timeout

import qualified Data.Map as Map

type KubeRunner = forall a.Eff '[Kube, IO] a -> IO a
type NatsRunner = forall a.Eff '[NatsOperation, IO] a -> IO a

testLabels = Map.fromList [("app", "test")]

testPodTemplateSpec :: V1PodTemplateSpec
testPodTemplateSpec = mkV1PodTemplateSpec
                      { v1PodTemplateSpecMetadata =
                          pure $ mkV1ObjectMeta{v1ObjectMetaLabels = pure testLabels}
                      , v1PodTemplateSpecSpec =
                          pure $ mkV1PodSpec [(mkV1Container "nginx"){v1ContainerImage = pure "nginx"}]
                      }

testLabelSelector :: V1LabelSelector
testLabelSelector = mkV1LabelSelector { v1LabelSelectorMatchLabels = pure $ testLabels}

testStatefulSet :: V1StatefulSet
testStatefulSet = mkV1StatefulSet
                  { v1StatefulSetMetadata =
                      pure mkV1ObjectMeta
                      { v1ObjectMetaName = pure "test-statefulset"
                      , v1ObjectMetaAnnotations = pure $ Map.fromList [("routes", "[{\"hostname\": \"foo.example.com\", \"port\": 8080}]")]}
                  , v1StatefulSetSpec =
                      pure
                      $ mkV1StatefulSetSpec testLabelSelector "test-service" testPodTemplateSpec
                  }

isStatefulSetReady :: FindElem Kube effs
                   => Name -> Namespace -> Eff effs Bool
isStatefulSetReady name ns = do
  ss <- getStatefulSet name ns
  return
    $ (v1StatefulSetStatus ss >>= v1StatefulSetStatusReadyReplicas)
    == (v1StatefulSetStatus ss >>= v1StatefulSetStatusCurrentReplicas)

findReady :: [V1StatefulSetCondition] -> Maybe V1StatefulSetCondition
findReady = find (\x -> v1StatefulSetConditionType x == "Ready")

isTrue :: V1StatefulSetCondition -> Bool
isTrue = (== "True") . v1StatefulSetConditionStatus

untilM :: Monad m => (a -> Bool) -> m a -> m a
untilM p m =
   let go =
         do x <- m
            if p x
              then return x
              else go
   in  go

handler :: Message -> IO ()
handler = undefined

-- TODO: Run this in a random namespace
spec :: KubeRunner -> NatsRunner -> Namespace -> Spec
spec kube nats ns = do
  after_ (void $ kube $ deleteStatefulSet (Name "test-statefulset") ns) $ do
    describe "Integration" $ do
      it "should publish routes" $ do
        kube $ createStatefulSet testStatefulSet ns
        kube $ untilM id $ isStatefulSetReady (Name "test-statefulset") ns
        subject <- either (const mzero) pure $ createSubject "router.>"
        recieved <- newEmptyMVar
        let natsHandler = putMVar recieved
        subId <- nats $ natsSubscribe subject natsHandler Nothing
        maybeMsg <- timeout (90 * seconds) $ takeMVar recieved
        case maybeMsg of
          Nothing -> expectationFailure "didn't recieve message in time"
          Just msg -> return ()

seconds = 1000000
