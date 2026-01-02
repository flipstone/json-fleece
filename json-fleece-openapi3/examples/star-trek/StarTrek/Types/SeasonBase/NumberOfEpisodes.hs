{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.SeasonBase.NumberOfEpisodes
  ( NumberOfEpisodes(..)
  , numberOfEpisodesSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfEpisodes = NumberOfEpisodes Integer
  deriving (Show, Eq)

numberOfEpisodesSchema :: FC.Fleece t => FC.Schema t NumberOfEpisodes
numberOfEpisodesSchema =
  FC.coerceSchema FC.integer