{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesFull.PublishedMonthTo
  ( PublishedMonthTo(..)
  , publishedMonthToSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedMonthTo = PublishedMonthTo Integer
  deriving (Show, Eq)

publishedMonthToSchema :: FC.Fleece schema => schema PublishedMonthTo
publishedMonthToSchema =
  FC.coerceSchema FC.integer