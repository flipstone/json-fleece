{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeriesBase.SeasonsCount
  ( SeasonsCount(..)
  , seasonsCountSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype SeasonsCount = SeasonsCount Integer
  deriving (Show, Eq)

seasonsCountSchema :: FC.Fleece schema => schema SeasonsCount
seasonsCountSchema =
  FC.coerceSchema FC.integer