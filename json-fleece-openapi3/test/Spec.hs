{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as T
import qualified Data.Yaml.Aeson as YA
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM

import qualified Fleece.CodeGenUtil as CGU
import qualified Fleece.CodeGenUtil.Config as Config
import Fleece.CodeGenUtil.Test (assertGoldenMatchesGenerated, loadTestConfig, testSpecSource)
import qualified Fleece.OpenApi3 as FOA3

main :: IO ()
main =
  -- Sequential because loading a test config sets an environment variable,
  -- which is not thread safe.
  HHM.defaultMain [HH.checkSequential (HH.Group "json-fleece-openapi3" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_testCasesExample", prop_testCasesExample)
  , ("prop_starTrekExample", prop_starTrekExample)
  , ("prop_selectedItemsExample", prop_selectedItemsExample)
  , ("prop_refAtUnsupportedPositionFails", prop_refAtUnsupportedPositionFails)
  , ("prop_refToMissingSchemaFails", prop_refToMissingSchemaFails)
  , ("prop_aliasCycleFails", prop_aliasCycleFails)
  , ("prop_refToOtherDocumentFails", prop_refToOtherDocumentFails)
  , ("prop_nonStringRefFails", prop_nonStringRefFails)
  , ("prop_refInPathExtensionSucceeds", prop_refInPathExtensionSucceeds)
  ]

testCasesFiles :: [(FilePath, BS8.ByteString)]
testCasesFiles =
  $(FileEmbed.embedDir "examples/test-cases")

prop_testCasesExample :: HH.Property
prop_testCasesExample =
  HH.withTests 1 . HH.property $ do
    config <- loadTestConfig (lookupOrFail testCasesFiles) "codegen.dhall"
    yaml <- lookupOrFail testCasesFiles (Config.inputFileName config)
    sourceValue <- YA.decodeThrow yaml
    (rawDocument, openApi) <- HH.evalEither (testSpecSource sourceValue)

    modules <-
      HH.evalEither $
        CGU.runCodeGen
          (Config.codeGenOptions config)
          (FOA3.generateOpenApiFleeceCode rawDocument openApi)

    assertGoldenMatchesGenerated (===) testCasesFiles modules

starTrekFiles :: [(FilePath, BS8.ByteString)]
starTrekFiles =
  $(FileEmbed.embedDir "examples/star-trek")

prop_starTrekExample :: HH.Property
prop_starTrekExample =
  HH.withTests 1 . HH.property $ do
    config <- loadTestConfig (lookupOrFail starTrekFiles) "codegen.dhall"
    yaml <- lookupOrFail starTrekFiles (Config.inputFileName config)
    sourceValue <- YA.decodeThrow yaml
    (rawDocument, openApi) <- HH.evalEither (testSpecSource sourceValue)

    modules <-
      HH.evalEither $
        CGU.runCodeGen
          (Config.codeGenOptions config)
          (FOA3.generateOpenApiFleeceCode rawDocument openApi)

    assertGoldenMatchesGenerated (===) starTrekFiles modules

lookupOrFail :: [(FilePath, a)] -> FilePath -> HH.PropertyT IO a
lookupOrFail haystack needle =
  case needle of
    '.' : '/' : rest ->
      -- If the path starts with './', drop it
      lookupOrFail haystack rest
    _ ->
      case lookup needle haystack of
        Nothing -> do
          HH.annotate ("failed to find " <> needle)
          HH.failure
        Just target ->
          pure target

selectedItemsFiles :: [(FilePath, BS8.ByteString)]
selectedItemsFiles =
  $(FileEmbed.embedDir "examples/selected-items-example")

prop_selectedItemsExample :: HH.Property
prop_selectedItemsExample =
  HH.withTests 1 . HH.property $ do
    config <- loadTestConfig (lookupOrFail selectedItemsFiles) "codegen.dhall"
    yaml <- lookupOrFail selectedItemsFiles (Config.inputFileName config)
    sourceValue <- YA.decodeThrow yaml
    (rawDocument, openApi) <- HH.evalEither (testSpecSource sourceValue)

    modules <-
      HH.evalEither $
        CGU.runCodeGen
          (Config.codeGenOptions config)
          (FOA3.generateOpenApiFleeceCode rawDocument openApi)

    assertGoldenMatchesGenerated (===) selectedItemsFiles modules

fixtureFiles :: [(FilePath, BS8.ByteString)]
fixtureFiles =
  $(FileEmbed.embedDir "test/fixtures")

prop_refAtUnsupportedPositionFails :: HH.Property
prop_refAtUnsupportedPositionFails =
  assertCodeGenFails "ref-at-unsupported-position.yaml" "paths./widgets"

prop_refToMissingSchemaFails :: HH.Property
prop_refToMissingSchemaFails =
  assertCodeGenFails "ref-to-missing-schema.yaml" "is not defined in this document"

prop_aliasCycleFails :: HH.Property
prop_aliasCycleFails =
  assertCodeGenFails "alias-cycle.yaml" "form a cycle of $ref aliases"

prop_nonStringRefFails :: HH.Property
prop_nonStringRefFails =
  assertCodeGenFails "non-string-ref.yaml" "$ref that is not a string"

prop_refToOtherDocumentFails :: HH.Property
prop_refToOtherDocumentFails =
  assertCodeGenFails
    "ref-to-other-document.yaml"
    "Only references to schemas in the same document are supported"

{- | A specification extension alongside the paths carries no meaning for code
generation, so a reference in one must not be rejected.
-}
prop_refInPathExtensionSucceeds :: HH.Property
prop_refInPathExtensionSucceeds =
  HH.withTests 1 . HH.property $ do
    config <- loadTestConfig (lookupOrFail fixtureFiles) "codegen.dhall"
    sourceValue <- lookupOrFail fixtureFiles "ref-in-path-extension.yaml" >>= YA.decodeThrow
    (rawDocument, openApi) <- HH.evalEither (testSpecSource sourceValue)

    modules <-
      HH.evalEither $
        CGU.runCodeGen
          (Config.codeGenOptions config)
          (FOA3.generateOpenApiFleeceCode rawDocument openApi)

    map fst modules
      === [ "Guard/Operations/GetWidgets.hs"
          , "Guard/Types/Thing.hs"
          ]

assertCodeGenFails :: FilePath -> T.Text -> HH.Property
assertCodeGenFails fixtureName expectedFragment =
  HH.withTests 1 . HH.property $ do
    config <- loadTestConfig (lookupOrFail fixtureFiles) "codegen.dhall"
    sourceValue <- lookupOrFail fixtureFiles fixtureName >>= YA.decodeThrow
    (rawDocument, openApi) <- HH.evalEither (testSpecSource sourceValue)

    case CGU.runCodeGen (Config.codeGenOptions config) (FOA3.generateOpenApiFleeceCode rawDocument openApi) of
      Right _modules -> do
        HH.annotate ("expected " <> fixtureName <> " to fail code generation")
        HH.failure
      Left err -> do
        let
          message = CGU.renderCodeGenError err

        HH.annotate (T.unpack message)
        HH.assert (T.isInfixOf expectedFragment message)
