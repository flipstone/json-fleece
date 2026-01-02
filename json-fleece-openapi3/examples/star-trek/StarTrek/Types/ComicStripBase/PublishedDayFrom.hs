{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicStripBase.PublishedDayFrom
  ( PublishedDayFrom(..)
  , publishedDayFromSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PublishedDayFrom = PublishedDayFrom Integer
  deriving (Show, Eq)

publishedDayFromSchema :: FC.Fleece t => FC.Schema t PublishedDayFrom
publishedDayFromSchema =
  FC.coerceSchema FC.integer