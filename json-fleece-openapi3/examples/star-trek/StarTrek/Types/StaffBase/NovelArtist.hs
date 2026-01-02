{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffBase.NovelArtist
  ( NovelArtist(..)
  , novelArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype NovelArtist = NovelArtist Bool
  deriving (Show, Eq)

novelArtistSchema :: FC.Fleece t => FC.Schema t NovelArtist
novelArtistSchema =
  FC.coerceSchema FC.boolean