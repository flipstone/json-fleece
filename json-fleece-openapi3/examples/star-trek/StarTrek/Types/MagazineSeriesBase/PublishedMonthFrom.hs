{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineSeriesBase.PublishedMonthFrom
  ( PublishedMonthFrom(..)
  , publishedMonthFromSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedMonthFrom = PublishedMonthFrom Integer
  deriving (Show, Eq)

publishedMonthFromSchema :: FC.Fleece t => FC.Schema t PublishedMonthFrom
publishedMonthFromSchema =
  FC.coerceSchema FC.integer