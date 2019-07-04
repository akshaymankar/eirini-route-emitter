{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module KubernetesExtras.Auth.GCP
  ( gcpAuth )
where

import Control.Applicative
import Data.Aeson                (Value)
import Data.Either.Combinators
import Data.Function             ((&))
import Data.Map                  (Map)
import Data.Text                 (Text)
import Data.JSONPath
import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.RFC3339
import Data.Typeable
import Kubernetes.Client
import Kubernetes.OpenAPI.Client
import Kubernetes.OpenAPI.Core
import KubernetesExtras.JSONPath
import Network.TLS
import System.Process.Typed
import Control.Concurrent.STM

import qualified Data.Aeson         as Aeson
import qualified Data.Map           as Map
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Lens.Micro         as L

-- TODO: Add support for scopes based token fetching
data GCPAuth = GCPAuth { gcpAccessToken :: TVar(Maybe Text)
                       , gcpTokenExpiry :: TVar(Maybe UTCTime)
                       , gcpCmd         :: ProcessConfig () () ()
                       , gcpTokenKey    :: [K8sPathElement]
                       , gcpExpiryKey   :: [K8sPathElement]
                       }

instance AuthMethod GCPAuth where
  applyAuthMethod config gcp req = do
    token <- getToken gcp >>= exceptEither
    pure
      $ setHeader req [("Authorization", "Bearer " <> (Text.encodeUtf8 token))]
      & L.set rAuthTypesL []

gcpAuth :: AuthInfo
        -> (ClientParams, KubernetesClientConfig)
        -> Maybe (IO (ClientParams, KubernetesClientConfig))
gcpAuth AuthInfo{authProvider = Just(AuthProviderConfig "gcp" (Just cfg))} (tls, kubecfg)
  = Just $ do
      configOfErr <- parseGCPAuthInfo cfg
      case configOfErr of
        Left e    -> error $ Text.unpack e
        Right gcp -> pure (tls, addAuthMethod kubecfg gcp)
gcpAuth _ _ = Nothing

exceptEither :: Either Text a -> IO a
exceptEither (Right a) = pure a
exceptEither (Left t)  = error (show t)

getToken :: GCPAuth -> IO (Either Text Text)
getToken g@(GCPAuth{..}) = getCurrentToken g
                           >>= maybe (fetchToken g) (return . Right)

getCurrentToken :: GCPAuth -> IO (Maybe Text)
getCurrentToken g@(GCPAuth{..})= do
  now <- getCurrentTime
  maybeExp <- atomically $ readTVar gcpTokenExpiry
  maybeToken <- atomically $ readTVar gcpAccessToken
  return $ do
    exp <- maybeExp
    if exp > now
      then maybeToken
      else Nothing

-- TODO: log if parsed expiry is invalid
fetchToken :: GCPAuth -> IO (Either Text Text)
fetchToken GCPAuth{..} = do
  _ <- print "lolololol"
  (stdOut, _) <- readProcess_ gcpCmd
  let credsJSON = Aeson.eitherDecode stdOut
                    & mapLeft Text.pack
      token =  runJSONPath gcpTokenKey =<< credsJSON
      expText = runJSONPath gcpExpiryKey =<< credsJSON
      exp = parseExpiryTime =<< expText
  atomically $ writeTVar gcpAccessToken (rightToMaybe token)
  atomically $ writeTVar gcpTokenExpiry (either (const Nothing) id exp)
  return token

parseGCPAuthInfo :: Map Text Text -> IO (Either Text GCPAuth)
parseGCPAuthInfo m = do
  gcpAccessToken <- atomically $ newTVar $ Map.lookup "access-token" m
  case maybe (pure Nothing) parseExpiryTime $ Map.lookup "expiry" m of
    (Left e) -> return $ Left e
    Right t -> do
      gcpTokenExpiry <- atomically $ newTVar t
      return $ do
        cmdPath <- Text.unpack <$> lookupEither m "cmd-path"
        cmdArgs <- Text.splitOn " " <$> lookupEither m "cmd-args"
        let gcpCmd = proc cmdPath (map Text.unpack cmdArgs)
            gcpTokenKey = readJSONPath m "token-key" [JSONPath [KeyChild "token_expiry"]]
            gcpExpiryKey = readJSONPath m "expiry-key" [JSONPath [KeyChild "access_token"]]
        pure $ GCPAuth{..}

lookupEither :: (Show key, Ord key) => Map key val -> key -> Either Text val
lookupEither m k = maybeToRight e $ Map.lookup k m
                   where e = "Couldn't find key: " <> (Text.pack $ show k) <> " in GCP auth info"

-- When Right, always returns Just
parseExpiryTime :: Text -> Either Text (Maybe UTCTime)
parseExpiryTime s = zonedTimeToUTC <$> parseTimeRFC3339 s
                    & maybeToRight ("failed to parse token expiry time " <> s)
                    & either Left (pure . Just)
