{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MixedInJustAdditionalPropertiesSchemaInline
  ( MixedInJustAdditionalPropertiesSchemaInline(..)
  , mixedInJustAdditionalPropertiesSchemaInlineSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Fleece.Core ((#*), (#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.MixedInJustAdditionalPropertiesSchemaInline.Bar as Bar
import qualified TestCases.Types.MixedInJustAdditionalPropertiesSchemaInline.Foo as Foo
import qualified TestCases.Types.MixedInJustAdditionalPropertiesSchemaInlineItem as MixedInJustAdditionalPropertiesSchemaInlineItem

data MixedInJustAdditionalPropertiesSchemaInline = MixedInJustAdditionalPropertiesSchemaInline
  { foo :: Maybe Foo.Foo
  , bar :: Maybe Bar.Bar
  , additionalProperties :: (Map.Map T.Text MixedInJustAdditionalPropertiesSchemaInlineItem.MixedInJustAdditionalPropertiesSchemaInlineItem)
  }
  deriving (Eq, Show)

mixedInJustAdditionalPropertiesSchemaInlineSchema :: FC.Fleece schema => schema MixedInJustAdditionalPropertiesSchemaInline
mixedInJustAdditionalPropertiesSchemaInlineSchema =
  FC.object $
    FC.constructor MixedInJustAdditionalPropertiesSchemaInline
      #+ FC.optional "foo" foo Foo.fooSchema
      #+ FC.optional "bar" bar Bar.barSchema
      #* FC.additionalFields additionalProperties MixedInJustAdditionalPropertiesSchemaInlineItem.mixedInJustAdditionalPropertiesSchemaInlineItemSchema