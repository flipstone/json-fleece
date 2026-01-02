{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DefaultTimeType
  ( DefaultTimeType(..)
  , defaultTimeTypeSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DefaultTimeType = DefaultTimeType Time.UTCTime
  deriving (Show, Eq)

defaultTimeTypeSchema :: FC.Fleece t => FC.Schema t DefaultTimeType
defaultTimeTypeSchema =
  FC.coerceSchema FC.utcTime