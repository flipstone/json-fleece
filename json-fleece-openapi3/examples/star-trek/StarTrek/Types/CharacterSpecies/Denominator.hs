{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.CharacterSpecies.Denominator
  ( Denominator(..)
  , denominatorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Denominator = Denominator Integer
  deriving (Show, Eq)

denominatorSchema :: FC.Fleece schema => schema Denominator
denominatorSchema =
  FC.coerceSchema FC.integer