{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.StaffFull.CalendarArtist
  ( CalendarArtist(..)
  , calendarArtistSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype CalendarArtist = CalendarArtist Bool
  deriving (Show, Eq)

calendarArtistSchema :: FC.Fleece t => FC.Schema t CalendarArtist
calendarArtistSchema =
  FC.coerceSchema FC.boolean