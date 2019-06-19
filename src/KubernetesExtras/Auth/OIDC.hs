{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module KubernetesExtras.Auth.OIDC
  (oidcAuth, OIDCCache, cachedOIDCAuth)
where

import Control.Applicative
import Control.Concurrent.STM
import Control.Exception
import Data.Either.Combinators
import Data.Function             ((&))
import Data.Map                  (Map)
import Data.Maybe
import Data.Text
import Data.Time.Clock.POSIX     (getPOSIXTime)
import Data.X509
import Kubernetes.Client         hiding (newManager)
import Kubernetes.OpenAPI.Core
import KubernetesExtras.TLSUtils
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.OAuth.OAuth2      as OAuth hiding (error)
import Network.TLS               as TLS
import URI.ByteString
import Web.JWT                   as JWT
import Web.OIDC.Client.Discovery as OIDC

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.Map               as Map
import qualified Data.Text              as Text
import qualified Data.Text.Encoding     as Text
import qualified Lens.Micro             as L

data OIDCAuth = OIDCAuth { issuerURL        :: Text
                         , clientID         :: Text
                         , clientSecret     :: Text
                         , tlsParams        :: TLS.ClientParams
                         , idTokenMVar      :: TVar(Maybe Text)
                         , refreshTokenMVar :: TVar(Maybe Text)
                         }

type OIDCCache = TVar (Map (Text, Text) OIDCAuth)

data CachedOIDCAuth = CachedOIDCAuth { cacheKey :: (Text, Text)
                                     , cache :: OIDCCache
                                     }

instance AuthMethod OIDCAuth where
  applyAuthMethod cfg oidc req = do
    token <- getToken oidc
    pure
      $ setHeader req [("Authorization", "Bearer " <> (Text.encodeUtf8 token))]
      & L.set rAuthTypesL []

-- TODO: Consider a token expired few seconds before actual expiry to account for time skew
getToken :: OIDCAuth -> IO Text
getToken o@(OIDCAuth{..}) = do
  now <- getPOSIXTime
  mgr <- newManager tlsManagerSettings
  idToken <- atomically $ readTVar idTokenMVar
  let maybeExp = idToken
                 & (>>= decode)
                 & (fmap claims)
                 & (>>= JWT.exp)
                 & (fmap secondsSinceEpoch)
      isValidToken = fromMaybe False (fmap (now <) maybeExp)
  if not isValidToken
    then fetchToken mgr o
    else return $ fromMaybe (error "impossible") idToken

fetchToken :: Manager -> OIDCAuth -> IO Text
fetchToken mgr o@(OIDCAuth{..}) = do
  maybeToken <- atomically $ readTVar refreshTokenMVar
  case maybeToken of
    Nothing -> error "cannot refresh id-token without a refresh token"
    Just token -> do
      tokenEndpoint <- fetchTokenEndpoint mgr o
      tokenURI <- exceptEither $ parseURI strictURIParserOptions (Text.encodeUtf8 tokenEndpoint)
      let oauth = OAuth2{ oauthClientId = clientID
                        , oauthClientSecret = clientSecret
                        , oauthAccessTokenEndpoint = tokenURI
                        , oauthOAuthorizeEndpoint = tokenURI
                        , oauthCallback = Nothing
                        }
      oauthToken <- refreshAccessToken mgr oauth (RefreshToken token)
                    >>= exceptEither
      case OAuth.idToken oauthToken of
        Nothing -> error "token response did not contain an id_token, either the scope \"openid\" wasn't requested upon login, or the provider doesn't support id_tokens as part of the refresh response."
        Just (IdToken t) -> do
          _ <- atomically $ writeTVar idTokenMVar (Just t)
          return t

fetchTokenEndpoint :: Manager -> OIDCAuth -> IO Text
fetchTokenEndpoint mgr OIDCAuth{..} = do
  discover issuerURL mgr
    & (fmap configuration)
    & (fmap tokenEndpoint)

exceptEither :: Show b => Either b a -> IO a
exceptEither (Right a) = pure a
exceptEither (Left t)  = error (show t)

oidcAuth :: AuthInfo
         -> (ClientParams, KubernetesClientConfig)
         -> Maybe (IO (ClientParams, KubernetesClientConfig))
oidcAuth AuthInfo{authProvider = Just(AuthProviderConfig "oidc" (Just cfg))} (tls, kubecfg)
  = Just
    $ parseOIDCAuthInfo cfg
    >>= either error (\oidc -> pure (tls, addAuthMethod kubecfg oidc))
oidcAuth _ _ = Nothing

-- TODO: Consider doing this whole function atomically, as two threads may miss the cache simultaneously
cachedOIDCAuth :: OIDCCache
               -> AuthInfo
               -> (ClientParams, KubernetesClientConfig)
               -> Maybe (IO (ClientParams, KubernetesClientConfig))
cachedOIDCAuth cache AuthInfo{authProvider = Just(AuthProviderConfig "oidc" (Just cfg))} (tls, kubecfg) = Just $ do
  m <- atomically $ readTVar cache
  o <- case findInCache m cfg of
    Left e -> error e
    Right (Just o) -> return o
    Right Nothing -> do
      o@(OIDCAuth{..}) <- either error pure =<< parseOIDCAuthInfo cfg
      let newCache = Map.insert (issuerURL, clientID) o m
      _ <- atomically $ swapTVar cache newCache
      return o
  pure (tls, addAuthMethod kubecfg o)

findInCache :: Map (Text, Text) a -> Map Text Text -> Either String (Maybe a)
findInCache cache cfg = do
  issuerURL <- lookupEither cfg "idp-issuer-url"
  clientID <- lookupEither cfg "client-id"
  return $ Map.lookup (issuerURL, clientID) cache

parseOIDCAuthInfo :: Map Text Text -> IO (Either String OIDCAuth)
parseOIDCAuthInfo m = do
  eitherTLSParams <- parseCA m
  idTokenMVar <- atomically $ newTVar $ Map.lookup "id-token" m
  refreshTokenMVar <- atomically $ newTVar $ Map.lookup "refresh-token" m
  return $ do
    tlsParams <- eitherTLSParams
    issuerURL <- lookupEither m "idp-issuer-url"
    clientID <- lookupEither m "client-id"
    clientSecret <- lookupEither m "client-secret"
    return OIDCAuth{..}

parseCA :: Map Text Text -> IO (Either String TLS.ClientParams)
parseCA m = do
  t <- defaultTLSClientParams
  fromMaybe (pure $ pure t) (parseCAFile t m <|> parseCAData t m)

parseCAFile :: TLS.ClientParams -> Map Text Text -> Maybe (IO (Either String TLS.ClientParams))
parseCAFile t m = do
  caFile <- Text.unpack <$> Map.lookup "idp-certificate-authority" m
  return $ updateClientParams t <$> BS.readFile caFile

parseCAData :: TLS.ClientParams -> Map Text Text -> Maybe (IO (Either String TLS.ClientParams))
parseCAData t m = do
  caText <- Map.lookup "idp-certificate-authority-data" m
  pure . pure
    $ (B64.decode $ Text.encodeUtf8 caText)
    >>= updateClientParams t

lookupEither :: (Show key, Ord key) => Map key val -> key -> Either String val
lookupEither m k = maybeToRight e $ Map.lookup k m
                   where e = "Couldn't find key: " <> show k <> " in OIDC auth info"
