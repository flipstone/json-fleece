{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardFull.ReleaseYear
  ( ReleaseYear(..)
  , releaseYearSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype ReleaseYear = ReleaseYear Integer
  deriving (Show, Eq)

releaseYearSchema :: FC.Fleece t => FC.Schema t ReleaseYear
releaseYearSchema =
  FC.coerceSchema FC.integer