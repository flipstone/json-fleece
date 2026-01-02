{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesBase.SeasonsCount
  ( SeasonsCount(..)
  , seasonsCountSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype SeasonsCount = SeasonsCount Integer
  deriving (Show, Eq)

seasonsCountSchema :: FC.Fleece t => FC.Schema t SeasonsCount
seasonsCountSchema =
  FC.coerceSchema FC.integer