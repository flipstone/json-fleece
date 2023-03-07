{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.ReferenceArtist
  ( ReferenceArtist(..)
  , referenceArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReferenceArtist = ReferenceArtist Bool
  deriving (Show, Eq)

referenceArtistSchema :: FC.Fleece schema => schema ReferenceArtist
referenceArtistSchema =
  FC.coerceSchema FC.boolean