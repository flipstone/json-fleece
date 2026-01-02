{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicsFull.CoverDay
  ( CoverDay(..)
  , coverDaySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype CoverDay = CoverDay Integer
  deriving (Show, Eq)

coverDaySchema :: FC.Fleece t => FC.Schema t CoverDay
coverDaySchema =
  FC.coerceSchema FC.integer