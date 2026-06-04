{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Operations.GetInventory.Response200Body.Meta
  ( Meta(..)
  , metaSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified SelectedItemsExample.Types.GetInventory.Response200Body.Meta.GeneratedAt as GeneratedAt

newtype Meta = Meta
  { generatedAt :: GeneratedAt.GeneratedAt
  }
  deriving (Eq, Show)

metaSchema :: FC.Fleece t => FC.Schema t Meta
metaSchema =
  FC.object $
    FC.constructor Meta
      #+ FC.required "generatedAt" generatedAt GeneratedAt.generatedAtSchema