{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookCollectionBase.PublishedDay
  ( PublishedDay(..)
  , publishedDaySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedDay = PublishedDay Integer
  deriving (Show, Eq)

publishedDaySchema :: FC.Fleece t => FC.Schema t PublishedDay
publishedDaySchema =
  FC.coerceSchema FC.integer