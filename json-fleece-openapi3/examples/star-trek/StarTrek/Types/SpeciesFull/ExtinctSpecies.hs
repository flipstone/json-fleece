{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesFull.ExtinctSpecies
  ( ExtinctSpecies(..)
  , extinctSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ExtinctSpecies = ExtinctSpecies Bool
  deriving (Show, Eq)

extinctSpeciesSchema :: FC.Fleece schema => schema ExtinctSpecies
extinctSpeciesSchema =
  FC.coerceSchema FC.boolean