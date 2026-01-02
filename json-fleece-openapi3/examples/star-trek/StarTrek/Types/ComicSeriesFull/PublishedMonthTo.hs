{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesFull.PublishedMonthTo
  ( PublishedMonthTo(..)
  , publishedMonthToSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedMonthTo = PublishedMonthTo Integer
  deriving (Show, Eq)

publishedMonthToSchema :: FC.Fleece t => FC.Schema t PublishedMonthTo
publishedMonthToSchema =
  FC.coerceSchema FC.integer