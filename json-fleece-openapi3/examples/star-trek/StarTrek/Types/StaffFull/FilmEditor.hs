{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.FilmEditor
  ( FilmEditor(..)
  , filmEditorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FilmEditor = FilmEditor Bool
  deriving (Show, Eq)

filmEditorSchema :: FC.Fleece t => FC.Schema t FilmEditor
filmEditorSchema =
  FC.coerceSchema FC.boolean