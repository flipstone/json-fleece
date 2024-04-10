{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.MixedInAdditionalPropertiesFalse
  ( MixedInAdditionalPropertiesFalse(..)
  , mixedInAdditionalPropertiesFalseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.MixedInAdditionalPropertiesFalse.Bar as Bar
import qualified TestCases.Types.MixedInAdditionalPropertiesFalse.Foo as Foo

data MixedInAdditionalPropertiesFalse = MixedInAdditionalPropertiesFalse
  { bar :: Maybe Bar.Bar
  , foo :: Maybe Foo.Foo
  }
  deriving (Eq, Show)

mixedInAdditionalPropertiesFalseSchema :: FC.Fleece schema => schema MixedInAdditionalPropertiesFalse
mixedInAdditionalPropertiesFalseSchema =
  FC.object $
    FC.constructor MixedInAdditionalPropertiesFalse
      #+ FC.optional "bar" bar Bar.barSchema
      #+ FC.optional "foo" foo Foo.fooSchema