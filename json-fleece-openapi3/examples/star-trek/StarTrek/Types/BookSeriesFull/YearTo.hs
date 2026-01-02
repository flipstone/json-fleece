{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.BookSeriesFull.YearTo
  ( YearTo(..)
  , yearToSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype YearTo = YearTo Integer
  deriving (Show, Eq)

yearToSchema :: FC.Fleece t => FC.Schema t YearTo
yearToSchema =
  FC.coerceSchema FC.integer