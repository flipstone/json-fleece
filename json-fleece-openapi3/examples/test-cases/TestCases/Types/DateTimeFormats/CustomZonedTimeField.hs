{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DateTimeFormats.CustomZonedTimeField
  ( CustomZonedTimeField(..)
  , customZonedTimeFieldSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Show)

newtype CustomZonedTimeField = CustomZonedTimeField Time.ZonedTime
  deriving (Show)

customZonedTimeFieldSchema :: FC.Fleece schema => schema CustomZonedTimeField
customZonedTimeFieldSchema =
  FC.coerceSchema (FC.zonedTimeWithFormat "zoned")