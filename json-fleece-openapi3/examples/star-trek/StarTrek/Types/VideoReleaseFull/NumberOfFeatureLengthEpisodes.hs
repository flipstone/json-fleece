{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull.NumberOfFeatureLengthEpisodes
  ( NumberOfFeatureLengthEpisodes(..)
  , numberOfFeatureLengthEpisodesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfFeatureLengthEpisodes = NumberOfFeatureLengthEpisodes Integer
  deriving (Show, Eq)

numberOfFeatureLengthEpisodesSchema :: FC.Fleece t => FC.Schema t NumberOfFeatureLengthEpisodes
numberOfFeatureLengthEpisodesSchema =
  FC.coerceSchema FC.integer