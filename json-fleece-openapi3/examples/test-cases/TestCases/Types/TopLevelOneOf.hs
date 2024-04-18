{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DataKinds #-}

module TestCases.Types.TopLevelOneOf
  ( TopLevelOneOf(..)
  , topLevelOneOfSchema
  ) where

import qualified Data.Text as T
import Fleece.Core ((#|))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Integer, Show)
import qualified Shrubbery as Shrubbery
import qualified TestCases.Types.AStringType as AStringType
import qualified TestCases.Types.FieldDescriptions as FieldDescriptions
import qualified TestCases.Types.MixedInJustAdditionalPropertiesSchemaInline as MixedInJustAdditionalPropertiesSchemaInline
import qualified TestCases.Types.Num2SchemaStartingWithNumber as Num2SchemaStartingWithNumber

newtype TopLevelOneOf = TopLevelOneOf (Shrubbery.Union
  '[ T.Text
   , Integer
   , [T.Text]
   , AStringType.AStringType
   , Num2SchemaStartingWithNumber.Num2SchemaStartingWithNumber
   , [FieldDescriptions.FieldDescriptions]
   , [[MixedInJustAdditionalPropertiesSchemaInline.MixedInJustAdditionalPropertiesSchemaInline]]
   ])
  deriving (Show, Eq)

topLevelOneOfSchema :: FC.Fleece schema => schema TopLevelOneOf
topLevelOneOfSchema =
  FC.coerceSchema $
    FC.unionNamed (FC.qualifiedName "TestCases.Types.TopLevelOneOf" "TopLevelOneOf") $
      FC.unionMember FC.text
        #| FC.unionMember FC.integer
        #| FC.unionMember (FC.list FC.text)
        #| FC.unionMember AStringType.aStringTypeSchema
        #| FC.unionMember Num2SchemaStartingWithNumber.num2SchemaStartingWithNumberSchema
        #| FC.unionMember (FC.list FieldDescriptions.fieldDescriptionsSchema)
        #| FC.unionMember (FC.list (FC.list MixedInJustAdditionalPropertiesSchemaInline.mixedInJustAdditionalPropertiesSchemaInlineSchema))