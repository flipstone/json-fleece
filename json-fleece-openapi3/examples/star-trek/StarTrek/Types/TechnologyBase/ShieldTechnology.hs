{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TechnologyBase.ShieldTechnology
  ( ShieldTechnology(..)
  , shieldTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ShieldTechnology = ShieldTechnology Bool
  deriving (Show, Eq)

shieldTechnologySchema :: FC.Fleece t => FC.Schema t ShieldTechnology
shieldTechnologySchema =
  FC.coerceSchema FC.boolean