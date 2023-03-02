{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.Cinematographer
  ( Cinematographer(..)
  , cinematographerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Cinematographer = Cinematographer Bool
  deriving (Show, Eq)

cinematographerSchema :: FC.Fleece schema => schema Cinematographer
cinematographerSchema =
  FC.coerceSchema FC.boolean