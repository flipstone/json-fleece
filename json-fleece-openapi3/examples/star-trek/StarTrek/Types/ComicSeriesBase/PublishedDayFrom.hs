{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesBase.PublishedDayFrom
  ( PublishedDayFrom(..)
  , publishedDayFromSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedDayFrom = PublishedDayFrom Integer
  deriving (Show, Eq)

publishedDayFromSchema :: FC.Fleece schema => schema PublishedDayFrom
publishedDayFromSchema =
  FC.coerceSchema FC.integer