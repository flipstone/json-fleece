{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialBase.ChemicalCompound
  ( ChemicalCompound(..)
  , chemicalCompoundSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ChemicalCompound = ChemicalCompound Bool
  deriving (Show, Eq)

chemicalCompoundSchema :: FC.Fleece t => FC.Schema t ChemicalCompound
chemicalCompoundSchema =
  FC.coerceSchema FC.boolean