{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SoundtrackFull.ReleaseDate
  ( ReleaseDate(..)
  , releaseDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype ReleaseDate = ReleaseDate Day
  deriving (Show, Eq)

releaseDateSchema :: FC.Fleece schema => schema ReleaseDate
releaseDateSchema =
  FC.coerceSchema FC.day