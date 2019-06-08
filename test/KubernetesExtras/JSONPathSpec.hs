{-# LANGUAGE OverloadedStrings #-}
module KubernetesExtras.JSONPathSpec where

import Control.Applicative
import Data.Aeson
import Data.Text
import Test.Hspec
import Test.Hspec.Attoparsec

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
          `shouldParse` InTheCurls [Field "foo"]
      it "should parse list of fields specified with dots" $ do
        ("{.foo.bar}" :: Text) ~> pathElementParser
          `shouldParse` InTheCurls [Field "foo", Field "bar"]
      it "should parse field specified with []" $ do
        ("{['foo']}" :: Text) ~> pathElementParser
          `shouldParse` InTheCurls [Field "foo"]
      it "should parse list of fields specified with []" $ do
        ("{['foo.bar']}" :: Text) ~> pathElementParser
          `shouldParse` InTheCurls [Field "foo", Field "bar"]
      it "should parse list of fields specified with multiple []" $ do
        ("{['foo']['bar']}" :: Text) ~> pathElementParser
          `shouldParse` InTheCurls [Field "foo", Field "bar"]
      it "should parse mixed list of fields" $ do
        ("{.foo['bar']}" :: Text) ~> pathElementParser
          `shouldParse` InTheCurls [Field "foo", Field "bar"]
      -- Foo
      it "should correctly parse text and curls" $ do
        ("kind is {.kind}" :: Text) ~> many pathElementParser
          `shouldParse` [PlainText "kind is ", InTheCurls [Field "kind"]]
      -- too complicated to handle right now
      xit "should fail if fields don't start with dot" $ do
        pathElementParser `shouldFailOn` ("{foo}" :: Text)
    describe "fieldParserWithDots" $ do
      it "should parse last field" $ do
        (".foo}" :: Text) ~> fieldParserWithDots
          `shouldParse` Field "foo"
      it "should parse non-last field" $ do
        (".foo.bar}" :: Text) ~> fieldParserWithDots
          `shouldParse` Field "foo"
      it "should parse last field before [" $ do
        (".foo['bar']" :: Text) ~> fieldParserWithDots
          `shouldParse` Field "foo"
    describe "fieldParserWithBrackets" $ do
      it "should parse field" $ do
        ("['foo']" :: Text) ~> fieldParserWithBrackets
          `shouldParse` [Field "foo"]
      it "should multiple fields" $ do
        ("['foo.bar']" :: Text) ~> fieldParserWithBrackets
          `shouldParse` [Field "foo", Field "bar"]
      it "should multiple fields starting with ." $ do
        ("['.foo.bar']" :: Text) ~> fieldParserWithBrackets
          `shouldParse` [Field "foo", Field "bar"]
    describe "runPathElement" $ do
      it "should return plain text" $ do
        runPathElement (PlainText "foo") undefined `shouldBe` Right "foo"
      it "should return value by field" $ do
        runPathElement (Field "foo") (object ["foo" .= ("bar" :: Text)])
          `shouldBe` Right "bar"
      it "should return error if field is not present" $ do
        runPathElement (Field "foo") (object ["not-foo" .= ("bar" :: Text)])
          `shouldBe` Left "key 'foo' not found in {\"not-foo\":\"bar\"}"
      it "should return value for empty InTheCurls" $ do
        runPathElement (InTheCurls []) "foo"
          `shouldBe` Right "foo"
      it "should return value for InTheCurls with one field" $ do
        runPathElement (InTheCurls [Field "foo"]) (object ["foo" .= ("bar" :: Text)])
          `shouldBe` Right "bar"
      it "should return value for InTheCurls with many field" $ do
        let o = (object ["foo" .= object [ "bar" .= ("baz" :: Text)]])
        runPathElement (InTheCurls [Field "foo", Field "bar"]) o
          `shouldBe` Right "baz"
    describe "runJSONPath" $ do
      it "should return empty if the path is empty" $ do
        let path = []
            value = object ["kind" .= ("List" :: Text)]
        runJSONPath path value
          `shouldBe` Right ""
      it "should run the whole json path" $ do
        let path = [PlainText "kind is ", InTheCurls [Field "kind"]]
            value = object ["kind" .= ("List" :: Text)]
        runJSONPath path value
          `shouldBe` Right "kind is List"
