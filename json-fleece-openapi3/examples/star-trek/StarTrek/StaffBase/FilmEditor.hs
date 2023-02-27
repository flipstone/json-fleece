{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.FilmEditor
  ( FilmEditor(..)
  , filmEditorSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype FilmEditor = FilmEditor Bool
  deriving (Show, Eq)

filmEditorSchema :: FC.Fleece schema => schema FilmEditor
filmEditorSchema =
  FC.coerceSchema FC.boolean