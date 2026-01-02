{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.LocalTimeType
  ( LocalTimeType(..)
  , localTimeTypeSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype LocalTimeType = LocalTimeType Time.LocalTime
  deriving (Show, Eq)

localTimeTypeSchema :: FC.Fleece t => FC.Schema t LocalTimeType
localTimeTypeSchema =
  FC.coerceSchema FC.localTime