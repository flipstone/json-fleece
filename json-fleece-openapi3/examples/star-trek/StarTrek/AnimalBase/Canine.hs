{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.AnimalBase.Canine
  ( Canine(..)
  , canineSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Canine = Canine Bool
  deriving (Show, Eq)

canineSchema :: FC.Fleece schema => schema Canine
canineSchema =
  FC.coerceSchema FC.boolean