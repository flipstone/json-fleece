{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Operations.Habitat.Option1
  ( Option1(..)
  , option1Schema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified SelectedItemsExample.Operations.Habitat.Option1.Land as Land

newtype Option1 = Option1
  { land :: Land.Land
  }
  deriving (Eq, Show)

option1Schema :: FC.Fleece t => FC.Schema t Option1
option1Schema =
  FC.object $
    FC.constructor Option1
      #+ FC.required "land" land Land.landSchema