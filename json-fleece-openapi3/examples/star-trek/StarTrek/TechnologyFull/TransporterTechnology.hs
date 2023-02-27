{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.TransporterTechnology
  ( TransporterTechnology(..)
  , transporterTechnologySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TransporterTechnology = TransporterTechnology Bool
  deriving (Show, Eq)

transporterTechnologySchema :: FC.Fleece schema => schema TransporterTechnology
transporterTechnologySchema =
  FC.coerceSchema FC.boolean