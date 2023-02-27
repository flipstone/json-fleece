{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.ComicCollectionBase.PublishedYear
  ( PublishedYear(..)
  , publishedYearSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedYear = PublishedYear Integer
  deriving (Show, Eq)

publishedYearSchema :: FC.Fleece schema => schema PublishedYear
publishedYearSchema =
  FC.coerceSchema FC.integer