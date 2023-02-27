{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull.Ds9Establishment
  ( Ds9Establishment(..)
  , ds9EstablishmentSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Ds9Establishment = Ds9Establishment Bool
  deriving (Show, Eq)

ds9EstablishmentSchema :: FC.Fleece schema => schema Ds9Establishment
ds9EstablishmentSchema =
  FC.coerceSchema FC.boolean