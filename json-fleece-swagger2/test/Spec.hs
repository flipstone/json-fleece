{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Char8 as BS8
import qualified Data.FileEmbed as FileEmbed
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM

import qualified Fleece.CodeGenUtil as CGU
import qualified Fleece.CodeGenUtil.Config as Config
import Fleece.CodeGenUtil.Test (assertGoldenMatchesGenerated, loadTestConfig)
import qualified Fleece.Swagger2 as FS2

main :: IO ()
main =
  HHM.defaultMain [HH.checkParallel (HH.Group "json-fleece-swagger2" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_uberExample", prop_uberExample)
  ]

uberFiles :: [(FilePath, BS8.ByteString)]
uberFiles =
  $(FileEmbed.embedDir "examples/uber")

prop_uberExample :: HH.Property
prop_uberExample =
  HH.withTests 1 . HH.property $ do
    config <- loadTestConfig (lookupOrFail uberFiles) "codegen.dhall"
    json <- lookupOrFail uberFiles (Config.inputFileName config)
    swagger <- HH.evalEither (Aeson.eitherDecodeStrict json)

    modules <-
      HH.evalEither $
        CGU.runCodeGen
          (Config.codeGenOptions config)
          (FS2.generateSwaggerFleeceCode swagger)

    assertGoldenMatchesGenerated (===) uberFiles modules

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
