{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.VideoReleaseFull.NumberOfEpisodes
  ( NumberOfEpisodes(..)
  , numberOfEpisodesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfEpisodes = NumberOfEpisodes Integer
  deriving (Show, Eq)

numberOfEpisodesSchema :: FC.Fleece schema => schema NumberOfEpisodes
numberOfEpisodesSchema =
  FC.coerceSchema FC.integer