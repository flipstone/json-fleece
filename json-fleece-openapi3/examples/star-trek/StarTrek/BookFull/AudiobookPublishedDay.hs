{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull.AudiobookPublishedDay
  ( AudiobookPublishedDay(..)
  , audiobookPublishedDaySchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AudiobookPublishedDay = AudiobookPublishedDay Integer
  deriving (Show, Eq)

audiobookPublishedDaySchema :: FC.Fleece schema => schema AudiobookPublishedDay
audiobookPublishedDaySchema =
  FC.coerceSchema FC.integer