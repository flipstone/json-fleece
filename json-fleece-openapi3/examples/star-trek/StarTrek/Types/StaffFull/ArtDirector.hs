{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.ArtDirector
  ( ArtDirector(..)
  , artDirectorSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ArtDirector = ArtDirector Bool
  deriving (Show, Eq)

artDirectorSchema :: FC.Fleece schema => schema ArtDirector
artDirectorSchema =
  FC.coerceSchema FC.boolean