{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.StuntPerformer
  ( StuntPerformer(..)
  , stuntPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype StuntPerformer = StuntPerformer Bool
  deriving (Show, Eq)

stuntPerformerSchema :: FC.Fleece schema => schema StuntPerformer
stuntPerformerSchema =
  FC.coerceSchema FC.boolean