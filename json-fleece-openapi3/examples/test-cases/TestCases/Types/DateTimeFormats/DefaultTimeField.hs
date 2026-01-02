{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DateTimeFormats.DefaultTimeField
  ( DefaultTimeField(..)
  , defaultTimeFieldSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DefaultTimeField = DefaultTimeField Time.UTCTime
  deriving (Show, Eq)

defaultTimeFieldSchema :: FC.Fleece t => FC.Schema t DefaultTimeField
defaultTimeFieldSchema =
  FC.coerceSchema FC.utcTime