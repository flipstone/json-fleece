{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicStripBase.PublishedYearTo
  ( PublishedYearTo(..)
  , publishedYearToSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedYearTo = PublishedYearTo Integer
  deriving (Show, Eq)

publishedYearToSchema :: FC.Fleece schema => schema PublishedYearTo
publishedYearToSchema =
  FC.coerceSchema FC.integer