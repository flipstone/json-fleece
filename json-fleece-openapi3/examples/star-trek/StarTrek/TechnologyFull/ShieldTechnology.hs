{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TechnologyFull.ShieldTechnology
  ( ShieldTechnology(..)
  , shieldTechnologySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ShieldTechnology = ShieldTechnology Bool
  deriving (Show, Eq)

shieldTechnologySchema :: FC.Fleece schema => schema ShieldTechnology
shieldTechnologySchema =
  FC.coerceSchema FC.boolean