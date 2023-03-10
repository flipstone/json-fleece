{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyFull.HolographicTechnology
  ( HolographicTechnology(..)
  , holographicTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HolographicTechnology = HolographicTechnology Bool
  deriving (Show, Eq)

holographicTechnologySchema :: FC.Fleece schema => schema HolographicTechnology
holographicTechnologySchema =
  FC.coerceSchema FC.boolean