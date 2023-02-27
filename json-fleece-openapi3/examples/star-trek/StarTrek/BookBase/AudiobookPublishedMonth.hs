{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookBase.AudiobookPublishedMonth
  ( AudiobookPublishedMonth(..)
  , audiobookPublishedMonthSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AudiobookPublishedMonth = AudiobookPublishedMonth Integer
  deriving (Show, Eq)

audiobookPublishedMonthSchema :: FC.Fleece schema => schema AudiobookPublishedMonth
audiobookPublishedMonthSchema =
  FC.coerceSchema FC.integer