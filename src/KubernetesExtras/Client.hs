{-# LANGUAGE OverloadedStrings #-}
module KubernetesExtras.Client where

import Control.Applicative          ((<|>))
import Control.Error.Safe
import Data.Aeson                   (Value)
import Data.Either.Combinators      (rightToMaybe)
import Data.Function                ((&))
import Data.HashMap.Strict          (HashMap)
import Data.Map                     (Map)
import Data.Maybe                   (fromMaybe)
import Data.Text                    (Text)
import Data.Text.Encoding           (encodeUtf8)
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.RFC3339
import Data.Yaml
import Kubernetes.Client.Config
import Kubernetes.Client.KubeConfig
import Kubernetes.OpenAPI.Core
import KubernetesExtras.JSONPath
import Network.HTTP.Client          (Manager)
import Network.TLS                  (ClientParams, credentialLoadX509,
                                     credentialLoadX509FromMemory)
import System.Environment
import System.Process.Typed

import qualified Data.Aeson             as Aeson
import qualified Data.Attoparsec.Text   as A
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.HashMap.Strict    as StrictMap
import qualified Data.Map               as Map
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Data.Text.IO           as Text

data KubeConfigSource = KubeConfigFile FilePath
                      | KubeConfigCluster

kubeClient :: KubeConfigSource -> IO (Manager, KubernetesClientConfig)
kubeClient (KubeConfigFile f) = do
  kubeConfigFile <- decodeFileThrow f
  uri <- rightZ (server <$> getCluster kubeConfigFile)
  t <- defaultTLSClientParams
       & fmap (tlsValidation kubeConfigFile )
       & fmap (addCACert kubeConfigFile)
  c <- newConfig & fmap (setMasterURI uri)
  (tlsParams, cfg) <-
    case getAuthInfo kubeConfigFile of
      Left _          -> return (t,c)
      Right (_, auth)-> applyAuthSettings auth (t, c)
  mgr <- newManager tlsParams
  return (mgr, cfg)
kubeClient (KubeConfigCluster) = Kubernetes.Client.Config.cluster

tlsValidation :: Config -> ClientParams -> ClientParams
tlsValidation cfg t = case getCluster cfg of
                        Left _ -> t
                        Right cluster -> case insecureSkipTLSVerify cluster of
                                           Just True -> disableServerCertValidation t
                                           _ -> t

addCACert :: Config -> ClientParams -> ClientParams
addCACert cfg t = fromMaybe t
                      $ ((flip setCAStore) t)
                        <$> ((rightToMaybe . parsePEMCerts)
                             =<< ((rightToMaybe . B64.decode . Text.encodeUtf8)
                                  =<< (certificateAuthorityData
                                       =<< (rightToMaybe $ getCluster cfg))))

applyAuthSettings :: AuthInfo -> (ClientParams, KubernetesClientConfig) -> IO (ClientParams, KubernetesClientConfig)
applyAuthSettings auth input = do
  currentTime <- getCurrentTime
  let maybeOutput = clientCertFileAuth auth input
                    <|> clientCertDataAuth auth input
                    <|> tokenAuth auth input
                    <|> tokenFileAuth auth input
                    <|> gcpAuth currentTime auth input
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

gcpAuth :: UTCTime -> AuthInfo -> (ClientParams, KubernetesClientConfig) -> Maybe (IO (ClientParams, KubernetesClientConfig))
gcpAuth currentTime AuthInfo{authProvider = Just(AuthProviderConfig "gcp" (Just cfg))} input = do
  cachedGCPAuth currentTime cfg input <|> newGCPAuth cfg input
gcpAuth _ _ _ = Nothing

cachedGCPAuth :: UTCTime -> Map Text Text -> (ClientParams, KubernetesClientConfig) -> Maybe(IO (ClientParams, KubernetesClientConfig))
cachedGCPAuth currentTime cfg (tlsParams, kubecfg) = do
  accessToken <- Map.lookup "access-token" cfg
  expiry <- zonedTimeToUTC <$> (parseTimeRFC3339 =<< Map.lookup "expiry" cfg)
  if expiry < currentTime
    then Nothing
    else Just $ pure (tlsParams, setTokenAuth accessToken kubecfg)

newGCPAuth :: Map Text Text -> (ClientParams, KubernetesClientConfig) -> Maybe(IO (ClientParams, KubernetesClientConfig))
newGCPAuth cfg (tlsParams, kubecfg) = do
  cmdPath <- Map.lookup "cmd-path" cfg
  cmdArgs <- Map.lookup "cmd-args" cfg
  let expiryKey = readJSONPath cfg "expiry-key" [InTheCurls [Field "token_expiry"]]
      tokenKey = readJSONPath cfg "token-key" [InTheCurls [Field "access_token"]]
  Just $ do
    let process = proc (Text.unpack cmdPath) (map Text.unpack (Text.splitOn " " cmdArgs))
    (stdOut, _) <- readProcess_ process
    case Aeson.decode stdOut :: Maybe Value of
      Nothing -> error "GCP auth cmd did not return valid JSON"
      Just creds -> do
        case runJSONPath tokenKey creds of
          Left e -> error ("Failed to read creds due to '" ++ Text.unpack e ++ "'")
          Right accessToken -> pure (tlsParams, setTokenAuth accessToken kubecfg)

readJSONPath :: Map Text Text -> Text -> JSONPath -> JSONPath
readJSONPath m key def = case Map.lookup key m of
                           Nothing -> def
                           Just str -> case A.parseOnly (jsonPathParser <* A.endOfInput) str of
                                         Left e  -> error e
                                         Right p -> p

jsonPath :: Text -> Maybe [Text]
jsonPath p = if Text.head p == '.'
             then Just $ Text.splitOn "." $ Text.tail p
             else Nothing

getText :: [Text] -> Value -> Maybe Text
getText [] (String s)       = Just s
getText (p:rest) (Object m) = getText rest =<< StrictMap.lookup p m
