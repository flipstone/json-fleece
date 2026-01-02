{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerBase.StuntPerformer
  ( StuntPerformer(..)
  , stuntPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype StuntPerformer = StuntPerformer Bool
  deriving (Show, Eq)

stuntPerformerSchema :: FC.Fleece t => FC.Schema t StuntPerformer
stuntPerformerSchema =
  FC.coerceSchema FC.boolean