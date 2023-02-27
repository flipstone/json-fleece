{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SpeciesBase.WarpCapableSpecies
  ( WarpCapableSpecies(..)
  , warpCapableSpeciesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype WarpCapableSpecies = WarpCapableSpecies Bool
  deriving (Show, Eq)

warpCapableSpeciesSchema :: FC.Fleece schema => schema WarpCapableSpecies
warpCapableSpeciesSchema =
  FC.coerceSchema FC.boolean