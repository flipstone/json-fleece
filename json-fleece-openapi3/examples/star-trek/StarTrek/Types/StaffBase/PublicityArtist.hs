{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.PublicityArtist
  ( PublicityArtist(..)
  , publicityArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PublicityArtist = PublicityArtist Bool
  deriving (Show, Eq)

publicityArtistSchema :: FC.Fleece t => FC.Schema t PublicityArtist
publicityArtistSchema =
  FC.coerceSchema FC.boolean