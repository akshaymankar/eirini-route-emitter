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

import qualified Data.Aeson         as Aeson
import qualified Data.Map           as Map
import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text
import qualified Lens.Micro         as L

data GCPAuth = GCPAuth { gcpAccessToken :: Maybe Text
                       , gcpTokenExpiry :: Maybe UTCTime
                       , gcpCmd         :: ProcessConfig () () ()
                       , gcpTokenKey    :: JSONPath
                       , gcpExpiryKey   :: JSONPath
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
  = Just $
      case parseGCPAuthInfo cfg of
        Left e    -> error $ Text.unpack e
        Right gcp -> pure (tls, addAuthMethod kubecfg gcp)
gcpAuth _ _ = Nothing

exceptEither :: Either Text a -> IO a
exceptEither (Right a) = pure a
exceptEither (Left t)  = error (show t)

getToken :: GCPAuth -> IO (Either Text Text)
getToken g@(GCPAuth{..}) = do
  now <- getCurrentTime
  maybe (fetchToken g) (pure . Right) $ getCurrentToken now g

getCurrentToken :: UTCTime -> GCPAuth -> Maybe Text
getCurrentToken now g@(GCPAuth{..})= do
  exp <- gcpTokenExpiry
  if exp > now
    then gcpAccessToken
    else Nothing

fetchToken :: GCPAuth -> IO (Either Text Text)
fetchToken GCPAuth{..} = do
    (stdOut, _) <- readProcess_ gcpCmd
    pure
      $ Aeson.eitherDecode stdOut
      & mapLeft Text.pack
      >>= runJSONPath gcpTokenKey

parseGCPAuthInfo :: Map Text Text -> Either Text GCPAuth
parseGCPAuthInfo m = do
  let gcpAccessToken = Map.lookup "access-token" m
  let expiryStr = Map.lookup "expiry" m
  gcpTokenExpiry <- case expiryStr of
                      Nothing -> pure Nothing
                      Just s -> fmap Just parseTimeInUTC s
                                & maybeToRight ("failed to parse token expiry time " <> s)
  cmdPath <- Text.unpack <$> lookupEither m "cmd-path"
  cmdArgs <- Text.splitOn " " <$> lookupEither m "cmd-args"
  let gcpCmd = proc cmdPath (map Text.unpack cmdArgs)
      gcpTokenKey = readJSONPath m "token-key" [InTheCurls [Field "token_expiry"]]
      gcpExpiryKey = readJSONPath m "expiry-key" [InTheCurls [Field "access_token"]]
  pure $ GCPAuth{..}

lookupEither :: (Show key, Ord key) => Map key val -> key -> Either Text val
lookupEither m k = maybeToRight e $ Map.lookup k m
                   where e = "Couldn't find key: " <> (Text.pack $ show k) <> " in GCP auth info"

parseTimeInUTC s = zonedTimeToUTC <$> parseTimeRFC3339 s
