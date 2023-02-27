{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.PerformerBase.FilmPerformer
  ( FilmPerformer(..)
  , filmPerformerSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FilmPerformer = FilmPerformer Bool
  deriving (Show, Eq)

filmPerformerSchema :: FC.Fleece schema => schema FilmPerformer
filmPerformerSchema =
  FC.coerceSchema FC.boolean