{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicStripFull.PublishedYearTo
  ( PublishedYearTo(..)
  , publishedYearToSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedYearTo = PublishedYearTo Integer
  deriving (Show, Eq)

publishedYearToSchema :: FC.Fleece schema => schema PublishedYearTo
publishedYearToSchema =
  FC.coerceSchema FC.integer