{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.ArtDirector
  ( ArtDirector(..)
  , artDirectorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ArtDirector = ArtDirector Bool
  deriving (Show, Eq)

artDirectorSchema :: FC.Fleece t => FC.Schema t ArtDirector
artDirectorSchema =
  FC.coerceSchema FC.boolean