{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ZonedTimeType
  ( ZonedTimeType(..)
  , zonedTimeTypeSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Show)

newtype ZonedTimeType = ZonedTimeType Time.ZonedTime
  deriving (Show)

zonedTimeTypeSchema :: FC.Fleece schema => schema ZonedTimeType
zonedTimeTypeSchema =
  FC.coerceSchema FC.zonedTime