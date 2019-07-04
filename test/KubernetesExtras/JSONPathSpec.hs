{-# LANGUAGE OverloadedStrings #-}
module KubernetesExtras.JSONPathSpec where

import Control.Applicative
import Data.Aeson
import Data.Text
import Test.Hspec
import Test.Hspec.Attoparsec
import Data.JSONPath

import KubernetesExtras.JSONPath

spec :: Spec
spec = do
  describe "JSONPath" $ do
    describe "Parser" $ do
      it "should parse plain text" $ do
        ("plain" :: Text) ~> pathElementParser
          `shouldParse` PlainText "plain"
      it "should not identify curls as plain text" $ do
        ("kind is {.kind}" :: Text) ~> pathElementParser
          `shouldParse` PlainText "kind is "
      it "should parse field specified with dot" $ do
        ("{.foo}" :: Text) ~> pathElementParser
          `shouldParse` JSONPath [KeyChild "foo"]
      it "should parse list of fields specified with dots" $ do
        ("{.foo.bar}" :: Text) ~> pathElementParser
          `shouldParse` JSONPath [KeyChild "foo", KeyChild "bar"]
      it "should parse field specified with []" $ do
        ("{['foo']}" :: Text) ~> pathElementParser
          `shouldParse` JSONPath [KeyChild "foo"]
      it "should parse list of fields specified with []" $ do
        ("{['foo.bar']}" :: Text) ~> pathElementParser
          `shouldParse` JSONPath [KeyChildren ["foo","bar"]]
      it "should parse list of fields specified with multiple []" $ do
        ("{['foo']['bar']}" :: Text) ~> pathElementParser
          `shouldParse` JSONPath [KeyChild "foo", KeyChild "bar"]
      it "should parse mixed list of fields" $ do
        ("{.foo['bar']}" :: Text) ~> pathElementParser
          `shouldParse` JSONPath [KeyChild "foo", KeyChild "bar"]
      it "should correctly parse text and curls" $ do
        ("kind is {.kind}" :: Text) ~> many pathElementParser
          `shouldParse` [PlainText "kind is ", JSONPath [KeyChild "kind"]]
      it "should fail if fields don't start with dot" $ do
        pathElementParser `shouldFailOn` ("{foo}" :: Text)
    describe "runJSONPath" $ do
      it "should return empty if the path is empty" $ do
        let path = []
            value = object ["kind" .= ("List" :: Text)]
        runJSONPath path value
          `shouldBe` Right ""
      it "should run the whole json path" $ do
        let path = [PlainText "kind is ", JSONPath [KeyChild "kind"]]
            value = object ["kind" .= ("List" :: Text)]
        runJSONPath path value
          `shouldBe` Right "kind is List"
