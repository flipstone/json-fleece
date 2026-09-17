{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Types.PetSummaryAlias
  ( PetSummaryAlias
  , petSummaryAliasSchema
  ) where

import qualified Fleece.Core as FC
import qualified SelectedItemsExample.Types.PetSummary as PetSummary

type PetSummaryAlias = PetSummary.PetSummary

petSummaryAliasSchema :: FC.Fleece t => FC.Schema t PetSummaryAlias
petSummaryAliasSchema =
  FC.coerceSchemaNamed
    (FC.qualifiedName "SelectedItemsExample.Types.PetSummaryAlias" "PetSummaryAlias")
    PetSummary.petSummarySchema