{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SpeciesBase.ExtinctSpecies
  ( ExtinctSpecies(..)
  , extinctSpeciesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ExtinctSpecies = ExtinctSpecies Bool
  deriving (Show, Eq)

extinctSpeciesSchema :: FC.Fleece t => FC.Schema t ExtinctSpecies
extinctSpeciesSchema =
  FC.coerceSchema FC.boolean