{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ConflictBase.EarthConflict
  ( EarthConflict(..)
  , earthConflictSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EarthConflict = EarthConflict Bool
  deriving (Show, Eq)

earthConflictSchema :: FC.Fleece t => FC.Schema t EarthConflict
earthConflictSchema =
  FC.coerceSchema FC.boolean