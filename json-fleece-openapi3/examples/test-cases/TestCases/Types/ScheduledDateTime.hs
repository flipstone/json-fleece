{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ScheduledDateTime
  ( ScheduledDateTime(..)
  , scheduledDateTimeSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified TestCases.Types.ScheduledDateTime.DatetimeType as DatetimeType

data ScheduledDateTime = ScheduledDateTime
  { datetimeType :: DatetimeType.DatetimeType -- ^ Type of the datetime
  }
  deriving (Eq, Show)

scheduledDateTimeSchema :: FC.Fleece t => FC.Schema t ScheduledDateTime
scheduledDateTimeSchema =
  FC.object $
    FC.constructor ScheduledDateTime
      #+ FC.required "datetime_type" datetimeType DatetimeType.datetimeTypeSchema