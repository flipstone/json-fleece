{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Types.PetSummaryAlias
  ( PetSummaryAlias(..)
  , petSummaryAliasSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Show)
import qualified SelectedItemsExample.Types.PetSummary as PetSummary

newtype PetSummaryAlias = PetSummaryAlias PetSummary.PetSummary
  deriving (Show, Eq)

petSummaryAliasSchema :: FC.Fleece t => FC.Schema t PetSummaryAlias
petSummaryAliasSchema =
  FC.coerceSchema PetSummary.petSummarySchema