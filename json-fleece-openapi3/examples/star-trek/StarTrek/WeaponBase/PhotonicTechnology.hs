{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.WeaponBase.PhotonicTechnology
  ( PhotonicTechnology(..)
  , photonicTechnologySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PhotonicTechnology = PhotonicTechnology Bool
  deriving (Show, Eq)

photonicTechnologySchema :: FC.Fleece schema => schema PhotonicTechnology
photonicTechnologySchema =
  FC.coerceSchema FC.boolean