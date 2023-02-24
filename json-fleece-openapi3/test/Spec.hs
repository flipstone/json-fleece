{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
  ( main
  ) where

import qualified Data.ByteString.Char8 as BS8
import qualified Data.FileEmbed as FileEmbed
import Data.Foldable (traverse_)
import qualified Data.List as List
import qualified Data.Set as Set
import qualified Data.Text.Encoding as Enc
import qualified Data.Yaml.Aeson as YA
import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM

import qualified Fleece.OpenApi3 as FOA3

main :: IO ()
main =
  HHM.defaultMain [HH.checkParallel (HH.Group "json-fleece-openapi3" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_starTrekExample", prop_starTrekExample)
  ]

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
        FOA3.CodeGenOptions
          { FOA3.moduleBaseName = "StarTrek"
          }

    case FOA3.generateFleeceCode codeGenOptions openApi of
      Left err -> do
        HH.annotate (show err)
        HH.failure
      Right modules -> do
        let
          expectedFileNames =
            Set.filter
              (\path -> List.isSuffixOf ".hs" path)
              (Set.fromList (map fst starTrekFiles))

          actualFileNames =
            Set.fromList (map fst modules)

          assertFileMatch (name, haskellCode) = do
            expected <- lookupOrFail name starTrekFiles
            -- split the lines here for the sake of the diff in the hedgehog
            -- output
            BS8.lines (Enc.encodeUtf8 (FOA3.renderText haskellCode))
              === BS8.lines expected

        actualFileNames === expectedFileNames
        traverse_ assertFileMatch modules

lookupOrFail :: FilePath -> [(FilePath, a)] -> HH.PropertyT IO a
lookupOrFail needle haystack =
  case lookup needle haystack of
    Nothing -> do
      HH.annotate ("failed to find " <> needle)
      HH.failure
    Just target ->
      pure target
