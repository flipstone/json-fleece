{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialBase.ChemicalCompound
  ( ChemicalCompound(..)
  , chemicalCompoundSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ChemicalCompound = ChemicalCompound Bool
  deriving (Show, Eq)

chemicalCompoundSchema :: FC.Fleece schema => schema ChemicalCompound
chemicalCompoundSchema =
  FC.coerceSchema FC.boolean