{-# LANGUAGE TemplateHaskell #-}
module Eirini.Route.Types
  ( RouteMessage(..)
  , RouteAnnotation(..)
  )
where

import Data.Aeson.TH
import Data.Text
import GHC.Word
import Data.Aeson.Casing

data RouteMessage = RouteMessage { uris              :: [Text]
                                 , app               :: Text
                                 , host              :: Text
                                 , port              :: Word32
                                 , privateInstanceId :: Text
                                 }
  deriving (Show, Eq)

data RouteAnnotation = RouteAnnotation { raHostname :: Text, raPort :: Word32}
  deriving (Show, Eq)

$(deriveJSON (aesonPrefix snakeCase) ''RouteAnnotation)
$(deriveJSON (defaultOptions{fieldLabelModifier = snakeCase}) ''RouteMessage)


