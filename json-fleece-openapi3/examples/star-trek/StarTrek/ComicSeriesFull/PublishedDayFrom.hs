{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicSeriesFull.PublishedDayFrom
  ( PublishedDayFrom(..)
  , publishedDayFromSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedDayFrom = PublishedDayFrom Integer
  deriving (Show, Eq)

publishedDayFromSchema :: FC.Fleece schema => schema PublishedDayFrom
publishedDayFromSchema =
  FC.coerceSchema FC.integer