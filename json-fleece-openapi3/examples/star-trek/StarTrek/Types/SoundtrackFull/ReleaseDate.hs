{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SoundtrackFull.ReleaseDate
  ( ReleaseDate(..)
  , releaseDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ReleaseDate = ReleaseDate Time.Day
  deriving (Show, Eq)

releaseDateSchema :: FC.Fleece t => FC.Schema t ReleaseDate
releaseDateSchema =
  FC.coerceSchema FC.day