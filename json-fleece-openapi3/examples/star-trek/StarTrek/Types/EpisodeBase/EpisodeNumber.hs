{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeBase.EpisodeNumber
  ( EpisodeNumber(..)
  , episodeNumberSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype EpisodeNumber = EpisodeNumber Integer
  deriving (Show, Eq)

episodeNumberSchema :: FC.Fleece t => FC.Schema t EpisodeNumber
episodeNumberSchema =
  FC.coerceSchema FC.integer