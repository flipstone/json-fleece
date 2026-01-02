{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.JustAdditionalPropertiesSchemaInlineItem
  ( JustAdditionalPropertiesSchemaInlineItem(..)
  , justAdditionalPropertiesSchemaInlineItemSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype JustAdditionalPropertiesSchemaInlineItem = JustAdditionalPropertiesSchemaInlineItem T.Text
  deriving (Show, Eq)

justAdditionalPropertiesSchemaInlineItemSchema :: FC.Fleece t => FC.Schema t JustAdditionalPropertiesSchemaInlineItem
justAdditionalPropertiesSchemaInlineItemSchema =
  FC.coerceSchema FC.text