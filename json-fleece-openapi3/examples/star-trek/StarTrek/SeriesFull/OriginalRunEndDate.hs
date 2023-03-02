{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesFull.OriginalRunEndDate
  ( OriginalRunEndDate(..)
  , originalRunEndDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OriginalRunEndDate = OriginalRunEndDate Time.Day
  deriving (Show, Eq)

originalRunEndDateSchema :: FC.Fleece schema => schema OriginalRunEndDate
originalRunEndDateSchema =
  FC.coerceSchema FC.day