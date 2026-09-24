{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as T
import qualified Fleece.CodeGenUtil as CGU
import qualified Fleece.CodeGenUtil.Config as Config
import Fleece.CodeGenUtil.Test (assertGoldenMatchesGenerated, loadTestConfig, testSpecSource)
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM

import qualified Fleece.Swagger2 as FS2

main :: IO ()
main =
  -- Sequential because loading a test config sets an environment variable,
  -- which is not thread safe.
  HHM.defaultMain [HH.checkSequential (HH.Group "json-fleece-swagger2" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_uberExample", prop_uberExample)
  , ("prop_definitionRefAliasGeneratesSynonym", prop_definitionRefAliasGeneratesSynonym)
  , ("prop_refToOtherDocumentFails", prop_refToOtherDocumentFails)
  ]

uberFiles :: [(FilePath, BS8.ByteString)]
uberFiles =
  $(FileEmbed.embedDir "examples/uber")

prop_uberExample :: HH.Property
prop_uberExample =
  HH.withTests 1 . HH.property $ do
    config <- loadTestConfig (lookupOrFail uberFiles) "codegen.dhall"
    json <- lookupOrFail uberFiles (Config.inputFileName config)
    sourceValue <- HH.evalEither (Aeson.eitherDecodeStrict json)
    (rawDocument, swagger) <- HH.evalEither (testSpecSource sourceValue)

    modules <-
      HH.evalEither $
        CGU.runCodeGen
          (Config.codeGenOptions config)
          (FS2.generateSwaggerFleeceCode rawDocument swagger)

    assertGoldenMatchesGenerated (===) uberFiles modules

fixtureFiles :: [(FilePath, BS8.ByteString)]
fixtureFiles =
  $(FileEmbed.embedDir "test/fixtures")

{- | A definition written as nothing but a @$ref@ names the definition it
points at. The reference prefix differs from OpenAPI's, so this covers the
Swagger dialect of the shared alias handling.
-}
prop_definitionRefAliasGeneratesSynonym :: HH.Property
prop_definitionRefAliasGeneratesSynonym =
  HH.withTests 1 . HH.property $ do
    modules <- generateFixture "definition-ref-alias.json"

    map fst modules
      === [ "Guard/Types/AString.hs"
          , "Guard/Types/AStringAlias.hs"
          ]

    aliasModule <- lookupOrFail modules "Guard/Types/AStringAlias.hs"
    HH.assert (T.isInfixOf "type AStringAlias = AString.AString" (CGU.renderText aliasModule))

{- | A reference spelled with OpenAPI's prefix names nothing in a Swagger
document.
-}
prop_refToOtherDocumentFails :: HH.Property
prop_refToOtherDocumentFails =
  HH.withTests 1 . HH.property $ do
    config <- loadTestConfig (lookupOrFail fixtureFiles) "codegen.dhall"
    sourceValue <-
      lookupOrFail fixtureFiles "ref-to-other-document.json"
        >>= HH.evalEither . Aeson.eitherDecodeStrict
    (rawDocument, swagger) <- HH.evalEither (testSpecSource sourceValue)

    case CGU.runCodeGen (Config.codeGenOptions config) (FS2.generateSwaggerFleeceCode rawDocument swagger) of
      Right _modules -> do
        HH.annotate "expected ref-to-other-document.json to fail code generation"
        HH.failure
      Left err -> do
        let
          message = CGU.renderCodeGenError err

        HH.annotate (T.unpack message)
        HH.assert
          (T.isInfixOf "Only references to schemas in the same document are supported" message)

generateFixture :: FilePath -> HH.PropertyT IO CGU.Modules
generateFixture fixtureName = do
  config <- loadTestConfig (lookupOrFail fixtureFiles) "codegen.dhall"
  sourceValue <-
    lookupOrFail fixtureFiles fixtureName >>= HH.evalEither . Aeson.eitherDecodeStrict
  (rawDocument, swagger) <- HH.evalEither (testSpecSource sourceValue)

  HH.evalEither $
    CGU.runCodeGen
      (Config.codeGenOptions config)
      (FS2.generateSwaggerFleeceCode rawDocument swagger)

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
