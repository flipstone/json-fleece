{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerFull.VideoGamePerformer
  ( VideoGamePerformer(..)
  , videoGamePerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VideoGamePerformer = VideoGamePerformer Bool
  deriving (Show, Eq)

videoGamePerformerSchema :: FC.Fleece schema => schema VideoGamePerformer
videoGamePerformerSchema =
  FC.coerceSchema FC.boolean