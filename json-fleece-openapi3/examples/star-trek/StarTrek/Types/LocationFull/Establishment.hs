{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.Establishment
  ( Establishment(..)
  , establishmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Establishment = Establishment Bool
  deriving (Show, Eq)

establishmentSchema :: FC.Fleece schema => schema Establishment
establishmentSchema =
  FC.coerceSchema FC.boolean