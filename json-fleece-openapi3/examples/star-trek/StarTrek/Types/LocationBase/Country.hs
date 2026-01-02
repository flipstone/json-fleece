{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.Country
  ( Country(..)
  , countrySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Country = Country Bool
  deriving (Show, Eq)

countrySchema :: FC.Fleece t => FC.Schema t Country
countrySchema =
  FC.coerceSchema FC.boolean