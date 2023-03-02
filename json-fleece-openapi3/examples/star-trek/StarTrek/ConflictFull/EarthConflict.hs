{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ConflictFull.EarthConflict
  ( EarthConflict(..)
  , earthConflictSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype EarthConflict = EarthConflict Bool
  deriving (Show, Eq)

earthConflictSchema :: FC.Fleece schema => schema EarthConflict
earthConflictSchema =
  FC.coerceSchema FC.boolean