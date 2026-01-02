{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.Cinematographer
  ( Cinematographer(..)
  , cinematographerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Cinematographer = Cinematographer Bool
  deriving (Show, Eq)

cinematographerSchema :: FC.Fleece t => FC.Schema t Cinematographer
cinematographerSchema =
  FC.coerceSchema FC.boolean