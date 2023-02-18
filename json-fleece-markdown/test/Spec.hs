{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import qualified Data.Text.Lazy as LT
import Hedgehog ((===))
import qualified Hedgehog as HH

-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
import qualified Hedgehog.Main as HHM

import Fleece.Examples (fooBarSchema)
import qualified Fleece.Markdown as FM

main :: IO ()
main =
  HHM.defaultMain [HH.checkParallel (HH.Group "json-fleece-markdown" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_objectMarkdown", prop_objectMarkdown)
  ]

prop_objectMarkdown :: HH.Property
prop_objectMarkdown =
  HH.withTests 1 . HH.property $
    FM.renderMarkdown fooBarSchema
      === LT.intercalate
        "\n"
        [ "# FooBar"
        , ""
        , "|Field|Key Required|Null Allowed|Type|"
        , "|---|---|---|---|"
        , "|foo|yes|no|string|"
        , "|bar|yes|no|number|"
        , ""
        ]
