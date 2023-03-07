{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookBase.AudiobookPublishedYear
  ( AudiobookPublishedYear(..)
  , audiobookPublishedYearSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AudiobookPublishedYear = AudiobookPublishedYear Integer
  deriving (Show, Eq)

audiobookPublishedYearSchema :: FC.Fleece schema => schema AudiobookPublishedYear
audiobookPublishedYearSchema =
  FC.coerceSchema FC.integer