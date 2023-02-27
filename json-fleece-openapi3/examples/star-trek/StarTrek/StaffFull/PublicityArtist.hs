{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffFull.PublicityArtist
  ( PublicityArtist(..)
  , publicityArtistSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PublicityArtist = PublicityArtist Bool
  deriving (Show, Eq)

publicityArtistSchema :: FC.Fleece schema => schema PublicityArtist
publicityArtistSchema =
  FC.coerceSchema FC.boolean