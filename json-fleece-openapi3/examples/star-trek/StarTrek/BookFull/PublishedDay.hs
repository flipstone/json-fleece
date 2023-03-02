{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.BookFull.PublishedDay
  ( PublishedDay(..)
  , publishedDaySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedDay = PublishedDay Integer
  deriving (Show, Eq)

publishedDaySchema :: FC.Fleece schema => schema PublishedDay
publishedDaySchema =
  FC.coerceSchema FC.integer