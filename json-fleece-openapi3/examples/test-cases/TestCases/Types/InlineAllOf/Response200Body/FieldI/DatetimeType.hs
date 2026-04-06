{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.InlineAllOf.Response200Body.FieldI.DatetimeType
  ( DatetimeType(..)
  , datetimeTypeSchema
  , datetimeTypeToText
  , datetimeTypeFromText
  ) where

import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (($), (<>), Bounded, Either, Enum, Eq, Ord, Show, String)

data DatetimeType
  = UtcDatetime
  deriving (Eq, Show, Ord, Enum, Bounded)

datetimeTypeToText :: DatetimeType -> T.Text
datetimeTypeToText v =
  T.pack $
    case v of
      UtcDatetime -> "utc_datetime"

datetimeTypeFromText :: T.Text -> Either String DatetimeType
datetimeTypeFromText txt =
  case T.unpack txt of
    "utc_datetime" -> Either.Right UtcDatetime
    v -> Either.Left $ "Unknown DatetimeType: " <> v

datetimeTypeSchema :: FC.Fleece t => FC.Schema t DatetimeType
datetimeTypeSchema =
  FC.boundedEnum datetimeTypeToText