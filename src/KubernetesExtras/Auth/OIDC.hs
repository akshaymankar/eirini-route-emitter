{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module KubernetesExtras.Auth.OIDC
  (oidcAuth)
where

import Control.Applicative
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.Either.Combinators
import Data.Function           ((&))
import Data.Map                (Map)
import Data.Maybe
import Data.Text
import Data.X509
import Kubernetes.Client
import Kubernetes.OpenAPI.Core
import Network.TLS             as TLS
import Web.JWT as JWT

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map               as Map
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Lens.Micro             as L

data OIDCAuth = OIDCAuth { issuerURL    :: Text
                         , clientID     :: Text
                         , clientSecret :: Text
                         , tlsParams    :: TLS.ClientParams
                         , idToken      :: Maybe Text
                         , refreshToken :: Maybe Text}

instance AuthMethod OIDCAuth where
  applyAuthMethod cfg oidc req = do
    token <- getToken oidc >>= exceptEither
    pure
      $ setHeader req [("Authorization", "Bearer " <> (Text.encodeUtf8 token))]
      & L.set rAuthTypesL []

getToken :: OIDCAuth -> IO (Either Text Text)
getToken o@(OIDCAuth{..}) = do
  now <- getPOSIXTime
  let maybeExp = idToken
                 & (>>= decode)
                 & (fmap claims)
                 & (>>= JWT.exp)
                 & (fmap secondsSinceEpoch)
      isValidToken = fromMaybe False (fmap (now <) maybeExp)
  if not isValidToken
    then fetchToken o
    else return $ (maybeToRight "impossible" idToken)

fetchToken :: OIDCAuth -> IO (Either Text Text)
fetchToken OIDCAuth{..} = undefined

exceptEither :: Either Text a -> IO a
exceptEither (Right a) = pure a
exceptEither (Left t)  = error (show t)

oidcAuth :: AuthInfo
         -> (ClientParams, KubernetesClientConfig)
         -> Maybe (IO (ClientParams, KubernetesClientConfig))
oidcAuth AuthInfo{authProvider = Just(AuthProviderConfig "oidc" (Just cfg))} (tls, kubecfg) = Just $ do
  parsedOIDC <- parseOIDCAuthInfo cfg
  case parsedOIDC of
    Left e -> error $ Text.unpack e
    Right oidc -> pure (tls, addAuthMethod kubecfg oidc)
oidcAuth _ _ = Nothing

parseOIDCAuthInfo :: Map Text Text -> IO (Either Text OIDCAuth)
parseOIDCAuthInfo m = do
  eitherTLSParams <- parseCA m
  return $ do
    tlsParams <- eitherTLSParams
    issuerURL <- lookupEither m "idp-issuer-url"
    clientID <- lookupEither m "client-id"
    clientSecret <- lookupEither m "client-secret"
    let idToken = Map.lookup "id-token" m
        refreshToken = Map.lookup "refresh-token" m
    return OIDCAuth{..}

parseCA :: Map Text Text -> IO (Either Text TLS.ClientParams)
parseCA m = do
  t <- defaultTLSClientParams
  fromMaybe (pure $ pure t) (parseCAFile t m <|> parseCAData t m)

parseCAFile :: TLS.ClientParams -> Map Text Text -> Maybe (IO (Either Text TLS.ClientParams))
parseCAFile t m = do
  caFile <- Text.unpack <$> Map.lookup "idp-certificate-authority" m
  return $ setCAFromFile t caFile

parseCAData :: TLS.ClientParams -> Map Text Text -> Maybe (IO (Either Text TLS.ClientParams))
parseCAData t m = do
  caText <- Map.lookup "idp-certificate-authority-data" m
  return $ setCAFromMemory t caText

setCAFromMemory :: TLS.ClientParams -> Text -> IO (Either Text TLS.ClientParams)
setCAFromMemory t caText = return $ do
  certs <-  parseCAText caText
  return $ setCAStore certs t

setCAFromFile :: TLS.ClientParams -> FilePath -> IO (Either Text TLS.ClientParams)
setCAFromFile t f = do
  caText <- B64.decode <$> BS.readFile f
  return $ do
    certs <- mapLeft Text.pack $ parsePEMCerts =<< caText
    return $ setCAStore certs t

parseCAText :: Text -> Either Text [SignedCertificate]
parseCAText cert = (B64.decode $ Text.encodeUtf8 cert)
                   >>= parsePEMCerts
                   & mapLeft Text.pack

lookupEither :: (Show key, Ord key) => Map key val -> key -> Either Text val
lookupEither m k = maybeToRight e $ Map.lookup k m
                   where e = "Couldn't find key: " <> (Text.pack $ show k) <> " in OIDC auth info"
