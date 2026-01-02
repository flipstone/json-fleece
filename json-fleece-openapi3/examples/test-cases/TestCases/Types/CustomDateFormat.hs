{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.CustomDateFormat
  ( CustomDateFormat(..)
  , customDateFormatSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype CustomDateFormat = CustomDateFormat Time.Day
  deriving (Show, Eq)

customDateFormatSchema :: FC.Fleece t => FC.Schema t CustomDateFormat
customDateFormatSchema =
  FC.coerceSchema (FC.dayWithFormat "%m/%d/%y")