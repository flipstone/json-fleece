{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.TransporterTechnology
  ( TransporterTechnology(..)
  , transporterTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype TransporterTechnology = TransporterTechnology Bool
  deriving (Show, Eq)

transporterTechnologySchema :: FC.Fleece t => FC.Schema t TransporterTechnology
transporterTechnologySchema =
  FC.coerceSchema FC.boolean