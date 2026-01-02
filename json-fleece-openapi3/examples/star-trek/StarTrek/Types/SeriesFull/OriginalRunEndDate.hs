{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesFull.OriginalRunEndDate
  ( OriginalRunEndDate(..)
  , originalRunEndDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OriginalRunEndDate = OriginalRunEndDate Time.Day
  deriving (Show, Eq)

originalRunEndDateSchema :: FC.Fleece t => FC.Schema t OriginalRunEndDate
originalRunEndDateSchema =
  FC.coerceSchema FC.day