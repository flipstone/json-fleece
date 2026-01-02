{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesFull.EpisodesCount
  ( EpisodesCount(..)
  , episodesCountSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype EpisodesCount = EpisodesCount Integer
  deriving (Show, Eq)

episodesCountSchema :: FC.Fleece t => FC.Schema t EpisodesCount
episodesCountSchema =
  FC.coerceSchema FC.integer