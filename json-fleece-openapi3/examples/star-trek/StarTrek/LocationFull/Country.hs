{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull.Country
  ( Country(..)
  , countrySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Country = Country Bool
  deriving (Show, Eq)

countrySchema :: FC.Fleece schema => schema Country
countrySchema =
  FC.coerceSchema FC.boolean