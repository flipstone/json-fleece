{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InlineYesAdditionalPropertiesWithOneOfNullablityInside.Foobar
  ( Foobar(..)
  , foobarSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Foobar = Foobar Bool
  deriving (Show, Eq)

foobarSchema :: FC.Fleece schema => schema Foobar
foobarSchema =
  FC.coerceSchema FC.boolean