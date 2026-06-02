{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Types.PetName
  ( PetName(..)
  , petNameSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PetName = PetName T.Text
  deriving (Show, Eq)

petNameSchema :: FC.Fleece t => FC.Schema t PetName
petNameSchema =
  FC.coerceSchema FC.text