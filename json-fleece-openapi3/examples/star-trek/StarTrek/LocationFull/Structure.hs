{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull.Structure
  ( Structure(..)
  , structureSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Structure = Structure Bool
  deriving (Show, Eq)

structureSchema :: FC.Fleece schema => schema Structure
structureSchema =
  FC.coerceSchema FC.boolean