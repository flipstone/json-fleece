{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Types.PetId
  ( PetId(..)
  , petIdSchema
  ) where

import qualified Data.Int as I
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PetId = PetId I.Int32
  deriving (Show, Eq)

petIdSchema :: FC.Fleece t => FC.Schema t PetId
petIdSchema =
  FC.coerceSchema FC.int32