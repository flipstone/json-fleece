{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MixedInJustAdditionalPropertiesSchemaInlineItem
  ( MixedInJustAdditionalPropertiesSchemaInlineItem(..)
  , mixedInJustAdditionalPropertiesSchemaInlineItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype MixedInJustAdditionalPropertiesSchemaInlineItem = MixedInJustAdditionalPropertiesSchemaInlineItem T.Text
  deriving (Show, Eq)

mixedInJustAdditionalPropertiesSchemaInlineItemSchema :: FC.Fleece schema => schema MixedInJustAdditionalPropertiesSchemaInlineItem
mixedInJustAdditionalPropertiesSchemaInlineItemSchema =
  FC.coerceSchema FC.text