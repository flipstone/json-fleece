{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.UTCDateTime
  ( UTCDateTime(..)
  , uTCDateTimeSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified TestCases.Types.UTCDateTime.DatetimeType as DatetimeType
import qualified TestCases.Types.UTCDateTime.DatetimeValue as DatetimeValue

data UTCDateTime = UTCDateTime
  { datetimeType :: DatetimeType.DatetimeType -- ^ Type of the datetime
  , datetimeValue :: DatetimeValue.DatetimeValue -- ^ UTC datetime value
  }
  deriving (Eq, Show)

uTCDateTimeSchema :: FC.Fleece schema => schema UTCDateTime
uTCDateTimeSchema =
  FC.object $
    FC.constructor UTCDateTime
      #+ FC.required "datetime_type" datetimeType DatetimeType.datetimeTypeSchema
      #+ FC.required "datetime_value" datetimeValue DatetimeValue.datetimeValueSchema