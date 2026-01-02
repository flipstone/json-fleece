{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DateTimeFormats.ZonedTimeField
  ( ZonedTimeField(..)
  , zonedTimeFieldSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Show)

newtype ZonedTimeField = ZonedTimeField Time.ZonedTime
  deriving (Show)

zonedTimeFieldSchema :: FC.Fleece t => FC.Schema t ZonedTimeField
zonedTimeFieldSchema =
  FC.coerceSchema FC.zonedTime