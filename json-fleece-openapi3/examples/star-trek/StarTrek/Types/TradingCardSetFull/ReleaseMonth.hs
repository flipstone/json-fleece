{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetFull.ReleaseMonth
  ( ReleaseMonth(..)
  , releaseMonthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ReleaseMonth = ReleaseMonth Integer
  deriving (Show, Eq)

releaseMonthSchema :: FC.Fleece t => FC.Schema t ReleaseMonth
releaseMonthSchema =
  FC.coerceSchema FC.integer