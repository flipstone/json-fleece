{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.GeographicalLocation
  ( GeographicalLocation(..)
  , geographicalLocationSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype GeographicalLocation = GeographicalLocation Bool
  deriving (Show, Eq)

geographicalLocationSchema :: FC.Fleece t => FC.Schema t GeographicalLocation
geographicalLocationSchema =
  FC.coerceSchema FC.boolean