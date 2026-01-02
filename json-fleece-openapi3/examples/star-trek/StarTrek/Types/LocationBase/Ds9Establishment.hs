{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.Ds9Establishment
  ( Ds9Establishment(..)
  , ds9EstablishmentSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Ds9Establishment = Ds9Establishment Bool
  deriving (Show, Eq)

ds9EstablishmentSchema :: FC.Fleece t => FC.Schema t Ds9Establishment
ds9EstablishmentSchema =
  FC.coerceSchema FC.boolean