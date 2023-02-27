{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.FileEmbed as FileEmbed
import qualified Data.Yaml.Aeson as YA
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM

import qualified Fleece.CodeGenUtil as CGU
import Fleece.CodeGenUtil.Test (assertGoldenMatchesGenerated)
import qualified Fleece.OpenApi3 as FOA3

main :: IO ()
main =
  HHM.defaultMain [HH.checkParallel (HH.Group "json-fleece-openapi3" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_testCasesExample", prop_testCasesExample)
  , ("prop_starTrekExample", prop_starTrekExample)
  ]

testCasesFiles :: [(FilePath, BS8.ByteString)]
testCasesFiles =
  $(FileEmbed.embedDir "examples/test-cases")

prop_testCasesExample :: HH.Property
prop_testCasesExample =
  HH.withTests 1 . HH.property $ do
    yaml <- lookupOrFail "test-cases.yaml" testCasesFiles
    openApi <- YA.decodeThrow yaml

    let
      codeGenOptions =
        CGU.CodeGenOptions
          { CGU.moduleBaseName = "TestCases"
          }

    modules <-
      HH.evalEither $
        CGU.runCodeGen codeGenOptions (FOA3.generateOpenApiFleeceCode openApi)

    assertGoldenMatchesGenerated (===) testCasesFiles modules

starTrekFiles :: [(FilePath, BS8.ByteString)]
starTrekFiles =
  $(FileEmbed.embedDir "examples/star-trek")

prop_starTrekExample :: HH.Property
prop_starTrekExample =
  HH.withTests 1 . HH.property $ do
    yaml <- lookupOrFail "star-trek.yaml" starTrekFiles
    openApi <- YA.decodeThrow yaml

    let
      codeGenOptions =
        CGU.CodeGenOptions
          { CGU.moduleBaseName = "StarTrek"
          }

    modules <-
      HH.evalEither $
        CGU.runCodeGen codeGenOptions (FOA3.generateOpenApiFleeceCode openApi)

    assertGoldenMatchesGenerated (===) starTrekFiles modules

lookupOrFail :: FilePath -> [(FilePath, a)] -> HH.PropertyT IO a
lookupOrFail needle haystack =
  case lookup needle haystack of
    Nothing -> do
      HH.annotate ("failed to find " <> needle)
      HH.failure
    Just target ->
      pure target
