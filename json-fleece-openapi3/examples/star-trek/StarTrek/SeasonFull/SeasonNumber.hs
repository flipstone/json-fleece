{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.SeasonFull.SeasonNumber
  ( SeasonNumber(..)
  , seasonNumberSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype SeasonNumber = SeasonNumber Integer
  deriving (Show, Eq)

seasonNumberSchema :: FC.Fleece schema => schema SeasonNumber
seasonNumberSchema =
  FC.coerceSchema FC.integer