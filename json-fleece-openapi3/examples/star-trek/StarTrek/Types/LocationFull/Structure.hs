{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.Structure
  ( Structure(..)
  , structureSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Structure = Structure Bool
  deriving (Show, Eq)

structureSchema :: FC.Fleece t => FC.Schema t Structure
structureSchema =
  FC.coerceSchema FC.boolean