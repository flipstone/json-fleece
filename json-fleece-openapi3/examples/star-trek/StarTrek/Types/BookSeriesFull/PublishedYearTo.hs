{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookSeriesFull.PublishedYearTo
  ( PublishedYearTo(..)
  , publishedYearToSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedYearTo = PublishedYearTo Integer
  deriving (Show, Eq)

publishedYearToSchema :: FC.Fleece t => FC.Schema t PublishedYearTo
publishedYearToSchema =
  FC.coerceSchema FC.integer