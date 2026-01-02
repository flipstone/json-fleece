{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerBase.AnimalPerformer
  ( AnimalPerformer(..)
  , animalPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AnimalPerformer = AnimalPerformer Bool
  deriving (Show, Eq)

animalPerformerSchema :: FC.Fleece t => FC.Schema t AnimalPerformer
animalPerformerSchema =
  FC.coerceSchema FC.boolean