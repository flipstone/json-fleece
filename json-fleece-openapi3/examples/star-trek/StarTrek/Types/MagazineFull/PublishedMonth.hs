{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MagazineFull.PublishedMonth
  ( PublishedMonth(..)
  , publishedMonthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedMonth = PublishedMonth Integer
  deriving (Show, Eq)

publishedMonthSchema :: FC.Fleece schema => schema PublishedMonth
publishedMonthSchema =
  FC.coerceSchema FC.integer