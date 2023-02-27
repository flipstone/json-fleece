{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBase.OriginalRunEndDate
  ( OriginalRunEndDate(..)
  , originalRunEndDateSchema
  ) where

import Data.Time (Day)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype OriginalRunEndDate = OriginalRunEndDate Day
  deriving (Show, Eq)

originalRunEndDateSchema :: FC.Fleece schema => schema OriginalRunEndDate
originalRunEndDateSchema =
  FC.coerceSchema FC.day