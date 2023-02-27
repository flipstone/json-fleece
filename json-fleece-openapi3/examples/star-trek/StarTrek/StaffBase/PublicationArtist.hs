{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.StaffBase.PublicationArtist
  ( PublicationArtist(..)
  , publicationArtistSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PublicationArtist = PublicationArtist Bool
  deriving (Show, Eq)

publicationArtistSchema :: FC.Fleece schema => schema PublicationArtist
publicationArtistSchema =
  FC.coerceSchema FC.boolean