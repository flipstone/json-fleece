{-# LANGUAGE OverloadedStrings #-}

module Main
  ( main
  ) where

import Hedgehog ((===))
import qualified Hedgehog as HH
import qualified Hedgehog.Main as HHM

import qualified Fleece.Examples as Examples

main :: IO ()
main =
  HHM.defaultMain $ [HH.checkParallel (HH.Group "json-fleece-examples" tests)]

tests :: [(HH.PropertyName, HH.Property)]
tests =
  [ ("prop_SchemaValidatorInfo_CustomValidatorObject", prop_SchemaValidatorInfo_CustomValidatorObject)
  ]

prop_SchemaValidatorInfo_CustomValidatorObject :: HH.Property
prop_SchemaValidatorInfo_CustomValidatorObject =
  HH.withTests 1 . HH.property $ do
    let
      info = Examples.schemaValidatorInfo Examples.customValidatorObjectExampleSchema
      expected =
        Examples.ValidatorInfo
          { Examples.validatorInfoCustomValidatorInfo = []
          , Examples.validatorInfoChildren =
              [ Examples.ValidatorInfo
                  { Examples.validatorInfoCustomValidatorInfo =
                      [ Examples.IntegralMaximum (-1)
                      ]
                  , Examples.validatorInfoChildren = []
                  }
              , Examples.ValidatorInfo
                  { Examples.validatorInfoCustomValidatorInfo =
                      [ Examples.IntegralMinimum 0
                      ]
                  , Examples.validatorInfoChildren = []
                  }
              ]
          }
    info === expected
