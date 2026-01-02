{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DateTimeFormats.LocalTimeField
  ( LocalTimeField(..)
  , localTimeFieldSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype LocalTimeField = LocalTimeField Time.LocalTime
  deriving (Show, Eq)

localTimeFieldSchema :: FC.Fleece t => FC.Schema t LocalTimeField
localTimeFieldSchema =
  FC.coerceSchema FC.localTime