{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalFull.Canine
  ( Canine(..)
  , canineSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Canine = Canine Bool
  deriving (Show, Eq)

canineSchema :: FC.Fleece schema => schema Canine
canineSchema =
  FC.coerceSchema FC.boolean