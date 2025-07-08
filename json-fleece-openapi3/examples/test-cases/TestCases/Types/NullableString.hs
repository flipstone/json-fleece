{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.NullableString
  ( NullableString(..)
  , nullableStringSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype NullableString = NullableString T.Text
  deriving (Show, Eq)

nullableStringSchema :: FC.Fleece schema => schema NullableString
nullableStringSchema =
  FC.coerceSchema FC.text