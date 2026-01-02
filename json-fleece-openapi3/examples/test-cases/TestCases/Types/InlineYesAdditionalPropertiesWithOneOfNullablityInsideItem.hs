{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem
  ( InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem(..)
  , inlineYesAdditionalPropertiesWithOneOfNullablityInsideItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem = InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem T.Text
  deriving (Show, Eq)

inlineYesAdditionalPropertiesWithOneOfNullablityInsideItemSchema :: FC.Fleece t => FC.Schema t InlineYesAdditionalPropertiesWithOneOfNullablityInsideItem
inlineYesAdditionalPropertiesWithOneOfNullablityInsideItemSchema =
  FC.coerceSchema FC.text