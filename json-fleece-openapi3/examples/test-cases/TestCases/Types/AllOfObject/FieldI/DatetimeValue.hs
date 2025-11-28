{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.AllOfObject.FieldI.DatetimeValue
  ( DatetimeValue(..)
  , datetimeValueSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DatetimeValue = DatetimeValue Time.UTCTime
  deriving (Show, Eq)

datetimeValueSchema :: FC.Fleece schema => schema DatetimeValue
datetimeValueSchema =
  FC.coerceSchema FC.utcTime