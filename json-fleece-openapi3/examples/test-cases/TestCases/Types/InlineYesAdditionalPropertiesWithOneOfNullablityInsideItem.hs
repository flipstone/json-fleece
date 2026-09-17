{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem
  ( InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem
  , inlineYesAdditionalPropertiesWithOneOfNullablityInsideItemSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Either)
import qualified TestCases.Types.NullableString as NullableString

type InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem = Either FC.Null NullableString.NullableString

inlineYesAdditionalPropertiesWithOneOfNullablityInsideItemSchema :: FC.Fleece t => FC.Schema t InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem
inlineYesAdditionalPropertiesWithOneOfNullablityInsideItemSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "TestCases.Types.InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem" "InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem")
    (FC.nullable NullableString.nullableStringSchema)