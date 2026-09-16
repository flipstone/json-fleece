{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Types.PetSummary
  ( PetSummary(..)
  , petSummarySchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PetSummary = PetSummary T.Text
  deriving (Show, Eq)

petSummarySchema :: FC.Fleece t => FC.Schema t PetSummary
petSummarySchema =
  FC.coerceSchema FC.text