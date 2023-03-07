{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookFull.AudiobookPublishedDay
  ( AudiobookPublishedDay(..)
  , audiobookPublishedDaySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AudiobookPublishedDay = AudiobookPublishedDay Integer
  deriving (Show, Eq)

audiobookPublishedDaySchema :: FC.Fleece schema => schema AudiobookPublishedDay
audiobookPublishedDaySchema =
  FC.coerceSchema FC.integer