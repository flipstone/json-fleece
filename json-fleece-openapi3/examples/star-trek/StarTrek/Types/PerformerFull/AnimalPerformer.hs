{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.AnimalPerformer
  ( AnimalPerformer(..)
  , animalPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AnimalPerformer = AnimalPerformer Bool
  deriving (Show, Eq)

animalPerformerSchema :: FC.Fleece schema => schema AnimalPerformer
animalPerformerSchema =
  FC.coerceSchema FC.boolean