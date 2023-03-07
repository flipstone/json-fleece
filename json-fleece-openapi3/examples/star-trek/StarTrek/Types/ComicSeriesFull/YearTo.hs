{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ComicSeriesFull.YearTo
  ( YearTo(..)
  , yearToSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype YearTo = YearTo Integer
  deriving (Show, Eq)

yearToSchema :: FC.Fleece schema => schema YearTo
yearToSchema =
  FC.coerceSchema FC.integer