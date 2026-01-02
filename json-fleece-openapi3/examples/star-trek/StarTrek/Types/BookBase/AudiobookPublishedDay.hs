{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookBase.AudiobookPublishedDay
  ( AudiobookPublishedDay(..)
  , audiobookPublishedDaySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AudiobookPublishedDay = AudiobookPublishedDay Integer
  deriving (Show, Eq)

audiobookPublishedDaySchema :: FC.Fleece t => FC.Schema t AudiobookPublishedDay
audiobookPublishedDaySchema =
  FC.coerceSchema FC.integer