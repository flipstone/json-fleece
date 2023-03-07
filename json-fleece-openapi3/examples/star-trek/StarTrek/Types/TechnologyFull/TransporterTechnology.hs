{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.TransporterTechnology
  ( TransporterTechnology(..)
  , transporterTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TransporterTechnology = TransporterTechnology Bool
  deriving (Show, Eq)

transporterTechnologySchema :: FC.Fleece schema => schema TransporterTechnology
transporterTechnologySchema =
  FC.coerceSchema FC.boolean