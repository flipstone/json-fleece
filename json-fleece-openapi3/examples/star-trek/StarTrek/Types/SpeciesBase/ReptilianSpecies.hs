{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesBase.ReptilianSpecies
  ( ReptilianSpecies(..)
  , reptilianSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReptilianSpecies = ReptilianSpecies Bool
  deriving (Show, Eq)

reptilianSpeciesSchema :: FC.Fleece t => FC.Schema t ReptilianSpecies
reptilianSpeciesSchema =
  FC.coerceSchema FC.boolean