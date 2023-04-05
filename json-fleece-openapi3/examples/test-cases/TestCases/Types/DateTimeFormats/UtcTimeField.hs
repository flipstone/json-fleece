{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DateTimeFormats.UtcTimeField
  ( UtcTimeField(..)
  , utcTimeFieldSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype UtcTimeField = UtcTimeField Time.UTCTime
  deriving (Show, Eq)

utcTimeFieldSchema :: FC.Fleece schema => schema UtcTimeField
utcTimeFieldSchema =
  FC.coerceSchema FC.utcTime