{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesBase.FeatureLengthEpisodesCount
  ( FeatureLengthEpisodesCount(..)
  , featureLengthEpisodesCountSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype FeatureLengthEpisodesCount = FeatureLengthEpisodesCount Integer
  deriving (Show, Eq)

featureLengthEpisodesCountSchema :: FC.Fleece t => FC.Schema t FeatureLengthEpisodesCount
featureLengthEpisodesCountSchema =
  FC.coerceSchema FC.integer