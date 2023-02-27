{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookBase.AudiobookPublishedYear
  ( AudiobookPublishedYear(..)
  , audiobookPublishedYearSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype AudiobookPublishedYear = AudiobookPublishedYear Integer
  deriving (Show, Eq)

audiobookPublishedYearSchema :: FC.Fleece schema => schema AudiobookPublishedYear
audiobookPublishedYearSchema =
  FC.coerceSchema FC.integer