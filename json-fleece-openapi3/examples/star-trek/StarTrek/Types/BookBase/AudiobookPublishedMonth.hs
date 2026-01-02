{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookBase.AudiobookPublishedMonth
  ( AudiobookPublishedMonth(..)
  , audiobookPublishedMonthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AudiobookPublishedMonth = AudiobookPublishedMonth Integer
  deriving (Show, Eq)

audiobookPublishedMonthSchema :: FC.Fleece t => FC.Schema t AudiobookPublishedMonth
audiobookPublishedMonthSchema =
  FC.coerceSchema FC.integer