{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeFull.EpisodeNumber
  ( EpisodeNumber(..)
  , episodeNumberSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype EpisodeNumber = EpisodeNumber Integer
  deriving (Show, Eq)

episodeNumberSchema :: FC.Fleece schema => schema EpisodeNumber
episodeNumberSchema =
  FC.coerceSchema FC.integer