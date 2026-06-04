{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Operations.Habitat.Option2
  ( Option2(..)
  , option2Schema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified SelectedItemsExample.Operations.Habitat.Option2.Water as Water

newtype Option2 = Option2
  { water :: Water.Water
  }
  deriving (Eq, Show)

option2Schema :: FC.Fleece t => FC.Schema t Option2
option2Schema =
  FC.object $
    FC.constructor Option2
      #+ FC.required "water" water Water.waterSchema