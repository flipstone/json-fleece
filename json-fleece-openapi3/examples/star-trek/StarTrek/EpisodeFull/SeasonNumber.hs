{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.EpisodeFull.SeasonNumber
  ( SeasonNumber(..)
  , seasonNumberSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype SeasonNumber = SeasonNumber Integer
  deriving (Show, Eq)

seasonNumberSchema :: FC.Fleece schema => schema SeasonNumber
seasonNumberSchema =
  FC.coerceSchema FC.integer