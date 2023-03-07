{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeriesBase.EpisodesCount
  ( EpisodesCount(..)
  , episodesCountSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype EpisodesCount = EpisodesCount Integer
  deriving (Show, Eq)

episodesCountSchema :: FC.Fleece schema => schema EpisodesCount
episodesCountSchema =
  FC.coerceSchema FC.integer