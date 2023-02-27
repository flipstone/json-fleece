{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookCollectionFull.PublishedMonth
  ( PublishedMonth(..)
  , publishedMonthSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedMonth = PublishedMonth Integer
  deriving (Show, Eq)

publishedMonthSchema :: FC.Fleece schema => schema PublishedMonth
publishedMonthSchema =
  FC.coerceSchema FC.integer