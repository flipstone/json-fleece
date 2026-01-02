{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.WeaponFull.PhotonicTechnology
  ( PhotonicTechnology(..)
  , photonicTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PhotonicTechnology = PhotonicTechnology Bool
  deriving (Show, Eq)

photonicTechnologySchema :: FC.Fleece t => FC.Schema t PhotonicTechnology
photonicTechnologySchema =
  FC.coerceSchema FC.boolean