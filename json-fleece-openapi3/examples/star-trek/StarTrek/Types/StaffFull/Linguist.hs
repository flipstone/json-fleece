{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.Linguist
  ( Linguist(..)
  , linguistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Linguist = Linguist Bool
  deriving (Show, Eq)

linguistSchema :: FC.Fleece t => FC.Schema t Linguist
linguistSchema =
  FC.coerceSchema FC.boolean