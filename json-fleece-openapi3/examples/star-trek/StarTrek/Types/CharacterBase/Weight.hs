{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterBase.Weight
  ( Weight(..)
  , weightSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Weight = Weight Integer
  deriving (Show, Eq)

weightSchema :: FC.Fleece schema => schema Weight
weightSchema =
  FC.coerceSchema FC.integer