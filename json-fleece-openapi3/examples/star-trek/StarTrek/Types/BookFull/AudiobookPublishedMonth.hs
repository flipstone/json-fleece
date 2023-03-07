{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookFull.AudiobookPublishedMonth
  ( AudiobookPublishedMonth(..)
  , audiobookPublishedMonthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AudiobookPublishedMonth = AudiobookPublishedMonth Integer
  deriving (Show, Eq)

audiobookPublishedMonthSchema :: FC.Fleece schema => schema AudiobookPublishedMonth
audiobookPublishedMonthSchema =
  FC.coerceSchema FC.integer