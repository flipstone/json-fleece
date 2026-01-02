{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookCollectionBase.PublishedMonth
  ( PublishedMonth(..)
  , publishedMonthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedMonth = PublishedMonth Integer
  deriving (Show, Eq)

publishedMonthSchema :: FC.Fleece t => FC.Schema t PublishedMonth
publishedMonthSchema =
  FC.coerceSchema FC.integer