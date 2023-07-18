{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MixedInJustAdditionalPropertiesSchemaRef
  ( MixedInJustAdditionalPropertiesSchemaRef(..)
  , mixedInJustAdditionalPropertiesSchemaRefSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Fleece.Core ((#*), (#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.AStringType as AStringType
import qualified TestCases.Types.MixedInJustAdditionalPropertiesSchemaRef.Bar as Bar
import qualified TestCases.Types.MixedInJustAdditionalPropertiesSchemaRef.Foo as Foo

data MixedInJustAdditionalPropertiesSchemaRef = MixedInJustAdditionalPropertiesSchemaRef
  { bar :: Maybe Bar.Bar
  , foo :: Maybe Foo.Foo
  , additionalProperties :: (Map.Map T.Text AStringType.AStringType)
  }
  deriving (Eq, Show)

mixedInJustAdditionalPropertiesSchemaRefSchema :: FC.Fleece schema => schema MixedInJustAdditionalPropertiesSchemaRef
mixedInJustAdditionalPropertiesSchemaRefSchema =
  FC.object $
    FC.constructor MixedInJustAdditionalPropertiesSchemaRef
      #+ FC.optional "bar" bar Bar.barSchema
      #+ FC.optional "foo" foo Foo.fooSchema
      #* FC.additionalFields additionalProperties AStringType.aStringTypeSchema