{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NullableBoolean
  ( NullableBoolean(..)
  , nullableBooleanSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype NullableBoolean = NullableBoolean Bool
  deriving (Show, Eq)

nullableBooleanSchema :: FC.Fleece t => FC.Schema t NullableBoolean
nullableBooleanSchema =
  FC.coerceSchema FC.boolean