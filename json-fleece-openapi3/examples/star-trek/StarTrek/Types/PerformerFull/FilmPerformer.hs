{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.PerformerFull.FilmPerformer
  ( FilmPerformer(..)
  , filmPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FilmPerformer = FilmPerformer Bool
  deriving (Show, Eq)

filmPerformerSchema :: FC.Fleece t => FC.Schema t FilmPerformer
filmPerformerSchema =
  FC.coerceSchema FC.boolean