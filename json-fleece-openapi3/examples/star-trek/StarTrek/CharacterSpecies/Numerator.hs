{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.CharacterSpecies.Numerator
  ( Numerator(..)
  , numeratorSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype Numerator = Numerator Integer
  deriving (Show, Eq)

numeratorSchema :: FC.Fleece schema => schema Numerator
numeratorSchema =
  FC.coerceSchema FC.integer