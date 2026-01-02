{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.StandInPerformer
  ( StandInPerformer(..)
  , standInPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype StandInPerformer = StandInPerformer Bool
  deriving (Show, Eq)

standInPerformerSchema :: FC.Fleece t => FC.Schema t StandInPerformer
standInPerformerSchema =
  FC.coerceSchema FC.boolean