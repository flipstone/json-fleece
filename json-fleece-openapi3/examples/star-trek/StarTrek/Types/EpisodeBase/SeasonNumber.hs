{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.EpisodeBase.SeasonNumber
  ( SeasonNumber(..)
  , seasonNumberSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype SeasonNumber = SeasonNumber Integer
  deriving (Show, Eq)

seasonNumberSchema :: FC.Fleece t => FC.Schema t SeasonNumber
seasonNumberSchema =
  FC.coerceSchema FC.integer