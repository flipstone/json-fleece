{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Text as T
import qualified Data.Yaml.Aeson as YA
import qualified Fleece.CodeGenUtil as CGU
import qualified Fleece.CodeGenUtil.Config as Config
import Fleece.CodeGenUtil.Test (assertGoldenMatchesGenerated, loadTestConfig, testSpecSource)
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM

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
  , ("prop_nonStringRefAtPathFails", prop_nonStringRefAtPathFails)
  , ("prop_nonStringSchemaRefFails", prop_nonStringSchemaRefFails)
  , ("prop_unionMemberCollapseFails", prop_unionMemberCollapseFails)
  , ("prop_aliasTypeOptionsFail", prop_aliasTypeOptionsFail)
  , ("prop_differentInheritedDiscriminatorsFail", prop_differentInheritedDiscriminatorsFail)
  , ("prop_conflictingInheritedDiscriminatorTagFails", prop_conflictingInheritedDiscriminatorTagFails)
  , ("prop_inheritedDiscriminatorMappingMismatchFails", prop_inheritedDiscriminatorMappingMismatchFails)
  , ("prop_refInPathExtensionSucceeds", prop_refInPathExtensionSucceeds)
  , ("prop_unresolvableRefInFilteredSchemaSucceeds", prop_unresolvableRefInFilteredSchemaSucceeds)
  , ("prop_refInFilteredPathSucceeds", prop_refInFilteredPathSucceeds)
  , ("prop_refInSelectedPathFails", prop_refInSelectedPathFails)
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
  assertCodeGenFails
    "ref-at-unsupported-position.yaml"
    "Found a $ref to \"#/paths/~1things\" at paths./widgets"

prop_refToMissingSchemaFails :: HH.Property
prop_refToMissingSchemaFails =
  assertCodeGenFails "ref-to-missing-schema.yaml" "is not defined in this document"

prop_aliasCycleFails :: HH.Property
prop_aliasCycleFails =
  assertCodeGenFails "alias-cycle.yaml" "form a cycle of $ref aliases"

prop_nonStringRefAtPathFails :: HH.Property
prop_nonStringRefAtPathFails =
  assertCodeGenFails
    "non-string-ref.yaml"
    "Found a $ref that is not a string at paths./widgets"

prop_nonStringSchemaRefFails :: HH.Property
prop_nonStringSchemaRefFails =
  assertCodeGenFails
    "non-string-schema-ref.yaml"
    "The schema \"Alias\" has a $ref that is not a string"

prop_unionMemberCollapseFails :: HH.Property
prop_unionMemberCollapseFails =
  assertCodeGenFails
    "union-member-collapse.yaml"
    "has more than one member of the same type"

prop_aliasTypeOptionsFail :: HH.Property
prop_aliasTypeOptionsFail =
  assertCodeGenFailsUsing
    "alias-type-options.dhall"
    "alias-type-options.yaml"
    "is generated as a type synonym"

prop_differentInheritedDiscriminatorsFail :: HH.Property
prop_differentInheritedDiscriminatorsFail =
  assertCodeGenFails
    "inherited-discriminator-different.yaml"
    "inherit discriminators with different property names through allOf: kind, petType"

prop_conflictingInheritedDiscriminatorTagFails :: HH.Property
prop_conflictingInheritedDiscriminatorTagFails =
  assertCodeGenFails
    "inherited-discriminator-conflicting-tag.yaml"
    "inherit discriminator mappings that map the same tag to different schemas: shared"

prop_inheritedDiscriminatorMappingMismatchFails :: HH.Property
prop_inheritedDiscriminatorMappingMismatchFails =
  assertCodeGenFails
    "inherited-discriminator-mapping-mismatch.yaml"
    "inherit a discriminator whose mapping does not list exactly those members"

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
  assertCodeGenProduces
    "codegen.dhall"
    "ref-in-path-extension.yaml"
    [ "Guard/Operations/GetWidgets.hs"
    , "Guard/Types/Thing.hs"
    ]

{- | A reference that cannot resolve is only a problem if the schema holding it
survives filtering.
-}
prop_unresolvableRefInFilteredSchemaSucceeds :: HH.Property
prop_unresolvableRefInFilteredSchemaSucceeds =
  assertCodeGenProduces
    "unresolvable-ref-filtered.dhall"
    "unresolvable-ref-filtered.yaml"
    [ "Guard/Operations/GetWidgets.hs"
    , "Guard/Types/Widget.hs"
    , "Guard/Types/Widget/Name.hs"
    ]

{- | A path item written as a reference is only a problem if that path is
selected.
-}
prop_refInFilteredPathSucceeds :: HH.Property
prop_refInFilteredPathSucceeds =
  assertCodeGenProduces
    "path-ref-filtered.dhall"
    "path-ref-filtered.yaml"
    ["Guard/Operations/GetThings.hs"]

prop_refInSelectedPathFails :: HH.Property
prop_refInSelectedPathFails =
  assertCodeGenFailsUsing
    "path-ref-selected.dhall"
    "path-ref-filtered.yaml"
    "Found a $ref to \"#/paths/~1things\" at paths./widgets"

assertCodeGenProduces :: FilePath -> FilePath -> [FilePath] -> HH.Property
assertCodeGenProduces configName fixtureName expectedModules =
  HH.withTests 1 . HH.property $ do
    config <- loadTestConfig (lookupOrFail fixtureFiles) configName
    sourceValue <- lookupOrFail fixtureFiles fixtureName >>= YA.decodeThrow
    (rawDocument, openApi) <- HH.evalEither (testSpecSource sourceValue)

    modules <-
      HH.evalEither $
        CGU.runCodeGen
          (Config.codeGenOptions config)
          (FOA3.generateOpenApiFleeceCode rawDocument openApi)

    map fst modules === expectedModules

assertCodeGenFails :: FilePath -> T.Text -> HH.Property
assertCodeGenFails =
  assertCodeGenFailsUsing "codegen.dhall"

assertCodeGenFailsUsing :: FilePath -> FilePath -> T.Text -> HH.Property
assertCodeGenFailsUsing configName fixtureName expectedFragment =
  HH.withTests 1 . HH.property $ do
    config <- loadTestConfig (lookupOrFail fixtureFiles) configName
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
