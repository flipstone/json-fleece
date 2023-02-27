{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBase.OriginalRunStartDate
  ( OriginalRunStartDate(..)
  , originalRunStartDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OriginalRunStartDate = OriginalRunStartDate Day
  deriving (Show, Eq)

originalRunStartDateSchema :: FC.Fleece schema => schema OriginalRunStartDate
originalRunStartDateSchema =
  FC.coerceSchema FC.day