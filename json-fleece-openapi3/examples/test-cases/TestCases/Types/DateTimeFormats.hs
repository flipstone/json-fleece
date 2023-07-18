{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DateTimeFormats
  ( DateTimeFormats(..)
  , dateTimeFormatsSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Maybe, Show)
import qualified TestCases.Types.DateTimeFormats.DefaultTimeField as DefaultTimeField
import qualified TestCases.Types.DateTimeFormats.LocalTimeField as LocalTimeField
import qualified TestCases.Types.DateTimeFormats.UtcTimeField as UtcTimeField
import qualified TestCases.Types.DateTimeFormats.ZonedTimeField as ZonedTimeField

data DateTimeFormats = DateTimeFormats
  { defaultTimeField :: Maybe DefaultTimeField.DefaultTimeField
  , utcTimeField :: Maybe UtcTimeField.UtcTimeField
  , zonedTimeField :: Maybe ZonedTimeField.ZonedTimeField
  , localTimeField :: Maybe LocalTimeField.LocalTimeField
  }
  deriving (Show)

dateTimeFormatsSchema :: FC.Fleece schema => schema DateTimeFormats
dateTimeFormatsSchema =
  FC.object $
    FC.constructor DateTimeFormats
      #+ FC.optional "defaultTimeField" defaultTimeField DefaultTimeField.defaultTimeFieldSchema
      #+ FC.optional "utcTimeField" utcTimeField UtcTimeField.utcTimeFieldSchema
      #+ FC.optional "zonedTimeField" zonedTimeField ZonedTimeField.zonedTimeFieldSchema
      #+ FC.optional "localTimeField" localTimeField LocalTimeField.localTimeFieldSchema