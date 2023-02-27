{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MagazineSeriesFull.PublishedMonthTo
  ( PublishedMonthTo(..)
  , publishedMonthToSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedMonthTo = PublishedMonthTo Integer
  deriving (Show, Eq)

publishedMonthToSchema :: FC.Fleece schema => schema PublishedMonthTo
publishedMonthToSchema =
  FC.coerceSchema FC.integer