{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.UtcTimeType
  ( UtcTimeType(..)
  , utcTimeTypeSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype UtcTimeType = UtcTimeType Time.UTCTime
  deriving (Show, Eq)

utcTimeTypeSchema :: FC.Fleece schema => schema UtcTimeType
utcTimeTypeSchema =
  FC.coerceSchema FC.utcTime