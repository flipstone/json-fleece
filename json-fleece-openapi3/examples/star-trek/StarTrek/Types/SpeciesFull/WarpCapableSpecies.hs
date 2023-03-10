{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesFull.WarpCapableSpecies
  ( WarpCapableSpecies(..)
  , warpCapableSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype WarpCapableSpecies = WarpCapableSpecies Bool
  deriving (Show, Eq)

warpCapableSpeciesSchema :: FC.Fleece schema => schema WarpCapableSpecies
warpCapableSpeciesSchema =
  FC.coerceSchema FC.boolean