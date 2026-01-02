{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineFull.PublishedYear
  ( PublishedYear(..)
  , publishedYearSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedYear = PublishedYear Integer
  deriving (Show, Eq)

publishedYearSchema :: FC.Fleece t => FC.Schema t PublishedYear
publishedYearSchema =
  FC.coerceSchema FC.integer