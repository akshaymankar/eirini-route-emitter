{-# LANGUAGE OverloadedStrings #-}
module KubernetesExtras.JSONPath where

import Control.Applicative
import Data.Aeson
import Data.Aeson.Text
import Data.Attoparsec.Text
import Data.Either.Combinators
import Data.Text
import Data.Text.Lazy (toStrict)
import Data.Map (Map)
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as HashMap
import qualified Data.Map as Map

data ArrayPos = ArrayPosInt Int
              | ArrayPosLast
  deriving (Show, Eq)

data PathElement = PlainText Text
                 | CurrentObject
                 | RecursiveDecent [PathElement]
                 | Wildcard
                 | Field Text
                 | ArrayElements { naStart :: Int
                                 , naEnd   :: ArrayPos
                                 , naStep  :: Int
                                 }
                 | Union [PathElement]
                 | Filter
                 | Range [PathElement]
                 | InTheCurls [PathElement]
  deriving (Show, Eq)

type JSONPath = [PathElement]

jsonPathParser :: Parser [PathElement]
jsonPathParser = many1 pathElementParser

pathElementParser :: Parser PathElement
pathElementParser = curlsParser <|> plainTextParser

plainTextParser :: Parser PathElement
plainTextParser = PlainText <$> takeWhile1 (/= '{')

curlsParser :: Parser PathElement
curlsParser = do
  _ <- char '{'
  fields <- Prelude.concat
            <$> many1 (fieldParserWithBrackets <|> many1 fieldParserWithDots)
  _ <- char '}'
  return $ InTheCurls fields

fieldParserWithBrackets :: Parser [PathElement]
fieldParserWithBrackets = do
  _ <- char '['
  _ <- char '\''
  fields <- many1 fieldParserWithDots
  _ <- char '\''
  _ <- char ']'
  return fields

fieldParserWithDots :: Parser PathElement
fieldParserWithDots = do
  _ <- (char '.' *> string "") <|> string ""
  field <- takeWhile1 $ notInClass ".}'["
  return $ Field field

runJSONPath :: [PathElement] -> Value -> Either Text Text
runJSONPath [] _ = pure ""
runJSONPath (e:es) v = do
  res <- runPathElement e v
  rest <- runJSONPath es v
  pure $ res <> rest

keyNotFoundMessage :: Text -> HashMap Text Value -> Text
keyNotFoundMessage key m = ("key '" <> key <> "' not found in "
                            <> (toStrict $ encodeToLazyText m))

runPathElement :: PathElement -> Value -> Either Text Text
runPathElement (PlainText t) _ = pure t
runPathElement (Field f) (Object o) = maybeToRight (keyNotFoundMessage f o)
                                      $ (jsonToText <$> HashMap.lookup f o)
runPathElement (InTheCurls []) v = pure $ jsonToText v
runPathElement (InTheCurls ((Field f):fs)) (Object o) = do
  v <- maybeToRight (keyNotFoundMessage f o) $ HashMap.lookup f o
  runPathElement (InTheCurls fs) v

jsonToText :: Value -> Text
jsonToText (String t) = t
jsonToText x = toStrict $ encodeToLazyText x

readJSONPath :: Map Text Text -> Text -> JSONPath -> JSONPath
readJSONPath m key def = case Map.lookup key m of
                           Nothing -> def
                           Just str -> case parseOnly (jsonPathParser <* endOfInput) str of
                                         Left e  -> error e
                                         Right p -> p
