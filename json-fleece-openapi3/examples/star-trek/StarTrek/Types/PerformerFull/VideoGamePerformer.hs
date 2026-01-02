{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.VideoGamePerformer
  ( VideoGamePerformer(..)
  , videoGamePerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype VideoGamePerformer = VideoGamePerformer Bool
  deriving (Show, Eq)

videoGamePerformerSchema :: FC.Fleece t => FC.Schema t VideoGamePerformer
videoGamePerformerSchema =
  FC.coerceSchema FC.boolean