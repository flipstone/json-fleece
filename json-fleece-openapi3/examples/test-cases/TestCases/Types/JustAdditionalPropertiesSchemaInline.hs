{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.JustAdditionalPropertiesSchemaInline
  ( JustAdditionalPropertiesSchemaInline(..)
  , justAdditionalPropertiesSchemaInlineSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified TestCases.Types.JustAdditionalPropertiesSchemaInlineItem as JustAdditionalPropertiesSchemaInlineItem

newtype JustAdditionalPropertiesSchemaInline = JustAdditionalPropertiesSchemaInline (Map.Map T.Text JustAdditionalPropertiesSchemaInlineItem.JustAdditionalPropertiesSchemaInlineItem)
  deriving (Show, Eq)

justAdditionalPropertiesSchemaInlineSchema :: FC.Fleece t => FC.Schema t JustAdditionalPropertiesSchemaInline
justAdditionalPropertiesSchemaInlineSchema =
  FC.coerceSchema (FC.map JustAdditionalPropertiesSchemaInlineItem.justAdditionalPropertiesSchemaInlineItemSchema)