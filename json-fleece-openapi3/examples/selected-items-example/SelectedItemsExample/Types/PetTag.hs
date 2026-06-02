{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Types.PetTag
  ( PetTag(..)
  , petTagSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype PetTag = PetTag T.Text
  deriving (Show, Eq)

petTagSchema :: FC.Fleece t => FC.Schema t PetTag
petTagSchema =
  FC.coerceSchema FC.text