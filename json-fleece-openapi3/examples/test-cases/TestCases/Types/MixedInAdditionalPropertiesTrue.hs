{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MixedInAdditionalPropertiesTrue
  ( MixedInAdditionalPropertiesTrue(..)
  , mixedInAdditionalPropertiesTrueSchema
  ) where

import qualified Data.Map as Map
import qualified Data.Text as T
import Fleece.Core ((#*), (#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.MixedInAdditionalPropertiesTrue.Bar as Bar
import qualified TestCases.Types.MixedInAdditionalPropertiesTrue.Foo as Foo

data MixedInAdditionalPropertiesTrue = MixedInAdditionalPropertiesTrue
  { foo :: Maybe Foo.Foo
  , bar :: Maybe Bar.Bar
  , additionalProperties :: (Map.Map T.Text FC.AnyJSON)
  }
  deriving (Eq, Show)

mixedInAdditionalPropertiesTrueSchema :: FC.Fleece schema => schema MixedInAdditionalPropertiesTrue
mixedInAdditionalPropertiesTrueSchema =
  FC.object $
    FC.constructor MixedInAdditionalPropertiesTrue
      #+ FC.optional "foo" foo Foo.fooSchema
      #+ FC.optional "bar" bar Bar.barSchema
      #* FC.additionalFields additionalProperties FC.anyJSON