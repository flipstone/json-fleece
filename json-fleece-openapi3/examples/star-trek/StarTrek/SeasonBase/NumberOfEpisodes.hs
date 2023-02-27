{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonBase.NumberOfEpisodes
  ( NumberOfEpisodes(..)
  , numberOfEpisodesSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype NumberOfEpisodes = NumberOfEpisodes Integer
  deriving (Show, Eq)

numberOfEpisodesSchema :: FC.Fleece schema => schema NumberOfEpisodes
numberOfEpisodesSchema =
  FC.coerceSchema FC.integer