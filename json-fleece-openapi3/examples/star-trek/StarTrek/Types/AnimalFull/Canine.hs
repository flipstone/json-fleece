{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.AnimalFull.Canine
  ( Canine(..)
  , canineSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Canine = Canine Bool
  deriving (Show, Eq)

canineSchema :: FC.Fleece t => FC.Schema t Canine
canineSchema =
  FC.coerceSchema FC.boolean