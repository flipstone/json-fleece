{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerFull.FilmPerformer
  ( FilmPerformer(..)
  , filmPerformerSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FilmPerformer = FilmPerformer Bool
  deriving (Show, Eq)

filmPerformerSchema :: FC.Fleece schema => schema FilmPerformer
filmPerformerSchema =
  FC.coerceSchema FC.boolean