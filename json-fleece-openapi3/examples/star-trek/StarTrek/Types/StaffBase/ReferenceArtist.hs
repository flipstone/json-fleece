{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.ReferenceArtist
  ( ReferenceArtist(..)
  , referenceArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype ReferenceArtist = ReferenceArtist Bool
  deriving (Show, Eq)

referenceArtistSchema :: FC.Fleece t => FC.Schema t ReferenceArtist
referenceArtistSchema =
  FC.coerceSchema FC.boolean