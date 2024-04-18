{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DateTimeFormats.CustomUtcTimeField
  ( CustomUtcTimeField(..)
  , customUtcTimeFieldSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype CustomUtcTimeField = CustomUtcTimeField Time.UTCTime
  deriving (Show, Eq)

customUtcTimeFieldSchema :: FC.Fleece schema => schema CustomUtcTimeField
customUtcTimeFieldSchema =
  FC.coerceSchema (FC.utcTimeWithFormat "utc")