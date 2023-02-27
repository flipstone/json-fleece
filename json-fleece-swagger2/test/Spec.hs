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
import Fleece.CodeGenUtil.Test (assertGoldenMatchesGenerated)
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
    json <- lookupOrFail "uber.json" uberFiles
    swagger <- HH.evalEither (Aeson.eitherDecodeStrict json)

    let
      codeGenOptions =
        CGU.CodeGenOptions
          { CGU.moduleBaseName = "Uber"
          }

    modules <-
      HH.evalEither $
        CGU.runCodeGen codeGenOptions (FS2.generateSwaggerFleeceCode swagger)

    assertGoldenMatchesGenerated (===) uberFiles modules

lookupOrFail :: FilePath -> [(FilePath, a)] -> HH.PropertyT IO a
lookupOrFail needle haystack =
  case lookup needle haystack of
    Nothing -> do
      HH.annotate ("failed to find " <> needle)
      HH.failure
    Just target ->
      pure target
