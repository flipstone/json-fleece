{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ScheduledDateTime.DatetimeType
  ( DatetimeType(..)
  , datetimeTypeSchema
  , datetimeTypeToText
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), Bounded, Enum, Eq, Ord, Show)

data DatetimeType
  = UtcDatetime
  deriving (Eq, Show, Ord, Enum, Bounded)

datetimeTypeToText :: DatetimeType -> T.Text
datetimeTypeToText v =
  T.pack $
    case v of
      UtcDatetime -> "utc_datetime"

datetimeTypeSchema :: FC.Fleece schema => schema DatetimeType
datetimeTypeSchema =
  FC.boundedEnum datetimeTypeToText