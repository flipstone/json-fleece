{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripFull.PublishedDayTo
  ( PublishedDayTo(..)
  , publishedDayToSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedDayTo = PublishedDayTo Integer
  deriving (Show, Eq)

publishedDayToSchema :: FC.Fleece schema => schema PublishedDayTo
publishedDayToSchema =
  FC.coerceSchema FC.integer