{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookSeriesBase.PublishedYearFrom
  ( PublishedYearFrom(..)
  , publishedYearFromSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedYearFrom = PublishedYearFrom Integer
  deriving (Show, Eq)

publishedYearFromSchema :: FC.Fleece t => FC.Schema t PublishedYearFrom
publishedYearFromSchema =
  FC.coerceSchema FC.integer