{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBase.StuntPerformer
  ( StuntPerformer(..)
  , stuntPerformerSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype StuntPerformer = StuntPerformer Bool
  deriving (Show, Eq)

stuntPerformerSchema :: FC.Fleece schema => schema StuntPerformer
stuntPerformerSchema =
  FC.coerceSchema FC.boolean