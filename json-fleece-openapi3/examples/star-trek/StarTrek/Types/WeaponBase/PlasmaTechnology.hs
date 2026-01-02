{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.WeaponBase.PlasmaTechnology
  ( PlasmaTechnology(..)
  , plasmaTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PlasmaTechnology = PlasmaTechnology Bool
  deriving (Show, Eq)

plasmaTechnologySchema :: FC.Fleece t => FC.Schema t PlasmaTechnology
plasmaTechnologySchema =
  FC.coerceSchema FC.boolean