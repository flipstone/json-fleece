{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponFull.PlasmaTechnology
  ( PlasmaTechnology(..)
  , plasmaTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PlasmaTechnology = PlasmaTechnology Bool
  deriving (Show, Eq)

plasmaTechnologySchema :: FC.Fleece schema => schema PlasmaTechnology
plasmaTechnologySchema =
  FC.coerceSchema FC.boolean