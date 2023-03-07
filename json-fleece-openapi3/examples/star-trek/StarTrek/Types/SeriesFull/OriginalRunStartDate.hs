{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesFull.OriginalRunStartDate
  ( OriginalRunStartDate(..)
  , originalRunStartDateSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OriginalRunStartDate = OriginalRunStartDate Time.Day
  deriving (Show, Eq)

originalRunStartDateSchema :: FC.Fleece schema => schema OriginalRunStartDate
originalRunStartDateSchema =
  FC.coerceSchema FC.day