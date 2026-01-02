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

customZonedTimeFieldSchema :: FC.Fleece t => FC.Schema t CustomZonedTimeField
customZonedTimeFieldSchema =
  FC.coerceSchema (FC.zonedTimeWithFormat "zoned")