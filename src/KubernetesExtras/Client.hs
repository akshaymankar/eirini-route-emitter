{-# LANGUAGE OverloadedStrings #-}
module KubernetesExtras.Client where

import Control.Applicative          ((<|>))
import Control.Concurrent.STM
import Control.Error.Safe
import Data.Either                  (fromRight)
import Data.Either.Combinators      (maybeToRight, rightToMaybe)
import Data.Function                ((&))
import Data.Map                     (Map)
import Data.Maybe                   (fromMaybe)
import Data.Text                    (Text)
import Data.Text.Encoding           (encodeUtf8)
import Data.Time.Clock
import Data.Yaml
import Kubernetes.Client.Config
import Kubernetes.Client.KubeConfig
import Kubernetes.OpenAPI.Core
import KubernetesExtras.Auth.GCP
import KubernetesExtras.Auth.OIDC
import KubernetesExtras.TLSUtils
import Network.HTTP.Client          (Manager)
import Network.TLS                  (ClientParams, credentialLoadX509,
                                     credentialLoadX509FromMemory)
import System.FilePath

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Data.Text.IO           as Text

data KubeConfigSource = KubeConfigFile FilePath
                      | KubeConfigCluster

kubeClient :: OIDCCache -> KubeConfigSource -> IO (Manager, KubernetesClientConfig)
kubeClient oidcCache (KubeConfigFile f) = do
  kubeConfigFile <- decodeFileThrow f
  uri <- rightZ (server <$> getCluster kubeConfigFile)
  t <- defaultTLSClientParams
       & fmap (tlsValidation kubeConfigFile)
       & fmap (addCACertData kubeConfigFile)
       & (>>= addCACertFile kubeConfigFile (takeDirectory f))
  c <- newConfig & fmap (setMasterURI uri)
  (tlsParams, cfg) <-
    case getAuthInfo kubeConfigFile of
      Left _          -> return (t,c)
      Right (_, auth)-> applyAuthSettings oidcCache auth (t, c)
  mgr <- newManager tlsParams
  return (mgr, cfg)
kubeClient _ (KubeConfigCluster) = Kubernetes.Client.Config.cluster

tlsValidation :: Config -> ClientParams -> ClientParams
tlsValidation cfg t = case getCluster cfg of
                        Left _ -> t
                        Right cluster -> case insecureSkipTLSVerify cluster of
                                           Just True -> disableServerCertValidation t
                                           _ -> t

-- TODO: Error if base64 decoding or PEM parsing fails
addCACertData :: Config -> ClientParams -> ClientParams
addCACertData cfg t = getCluster cfg
                      & (>>= (maybeToRight "cert not provided" . certificateAuthorityData))
                      & (>>= B64.decode . Text.encodeUtf8 )
                      & (>>= updateClientParams t)
                      & (fromRight t)

addCACertFile :: Config -> FilePath -> ClientParams -> IO ClientParams
addCACertFile cfg dir t = do
  let certFile = getCluster cfg
                 & (>>= maybeToRight "cert file not provided" . certificateAuthority)
                 & (fmap Text.unpack)
                 & (fmap (dir </>))
  case certFile of
    Left _ -> return t
    Right f -> do
      certText <- BS.readFile f
      return
        $ updateClientParams t certText
        & (fromRight t)

applyAuthSettings :: OIDCCache
                  -> AuthInfo
                  -> (ClientParams, KubernetesClientConfig)
                  -> IO (ClientParams, KubernetesClientConfig)
applyAuthSettings oidcCache auth input = do
  let maybeOutput = clientCertFileAuth auth input
                    <|> clientCertDataAuth auth input
                    <|> tokenAuth auth input
                    <|> tokenFileAuth auth input
                    <|> gcpAuth auth input
                    <|> cachedOIDCAuth oidcCache auth input
  case maybeOutput of
     Nothing -> return input
     Just x  -> x

clientCertFileAuth :: AuthInfo -> (ClientParams, KubernetesClientConfig) -> Maybe (IO (ClientParams, KubernetesClientConfig))
clientCertFileAuth auth (tlsParams, cfg) = do
  certFile <- clientCertificate auth
  keyFile <- clientKey auth
  return $ do
    cert <- credentialLoadX509 certFile keyFile >>= either error return
    let newParams = (setClientCert cert tlsParams)
        newCfg = (disableValidateAuthMethods cfg)
    return (newParams, newCfg)

clientCertDataAuth :: AuthInfo -> (ClientParams, KubernetesClientConfig) -> Maybe (IO (ClientParams, KubernetesClientConfig))
clientCertDataAuth auth (tlsParams, cfg) = do
  certData <- encodeUtf8 <$> clientCertificateData auth
  keyData <- encodeUtf8 <$> clientKeyData auth
  cert <- rightToMaybe $ B64.decode certData
  key <- rightToMaybe $ B64.decode keyData
  cert <- rightToMaybe $ credentialLoadX509FromMemory cert key
  let newParams = (setClientCert cert tlsParams)
      newCfg = (disableValidateAuthMethods cfg)
  return $ return (newParams, newCfg)

tokenAuth :: AuthInfo -> (ClientParams, KubernetesClientConfig) -> Maybe (IO (ClientParams, KubernetesClientConfig))
tokenAuth auth (tlsParams, cfg) = do
  t <- token auth
  return $ return (tlsParams, setTokenAuth t cfg)

tokenFileAuth :: AuthInfo -> (ClientParams, KubernetesClientConfig) -> Maybe (IO (ClientParams, KubernetesClientConfig))
tokenFileAuth auth (tlsParams, cfg) = do
  file <- tokenFile auth
  return $ do
    t <- Text.readFile file
    return (tlsParams, setTokenAuth t cfg)
