{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ObjectWithStringOrNullableBooleanField
  ( ObjectWithStringOrNullableBooleanField(..)
  , objectWithStringOrNullableBooleanFieldSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.AStringOrNullableBoolean as AStringOrNullableBoolean

newtype ObjectWithStringOrNullableBooleanField = ObjectWithStringOrNullableBooleanField
  { value :: Maybe AStringOrNullableBoolean.AStringOrNullableBoolean
  }
  deriving (Eq, Show)

objectWithStringOrNullableBooleanFieldSchema :: FC.Fleece t => FC.Schema t ObjectWithStringOrNullableBooleanField
objectWithStringOrNullableBooleanFieldSchema =
  FC.object $
    FC.constructor ObjectWithStringOrNullableBooleanField
      #+ FC.optional "value" value AStringOrNullableBoolean.aStringOrNullableBooleanSchema