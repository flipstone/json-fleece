{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterSpecies.Denominator
  ( Denominator(..)
  , denominatorSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Denominator = Denominator Integer
  deriving (Show, Eq)

denominatorSchema :: FC.Fleece schema => schema Denominator
denominatorSchema =
  FC.coerceSchema FC.integer