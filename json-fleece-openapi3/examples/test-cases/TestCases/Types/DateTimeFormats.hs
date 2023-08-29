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
  { zonedTimeField :: Maybe ZonedTimeField.ZonedTimeField
  , defaultTimeField :: Maybe DefaultTimeField.DefaultTimeField
  , localTimeField :: Maybe LocalTimeField.LocalTimeField
  , utcTimeField :: Maybe UtcTimeField.UtcTimeField
  }
  deriving (Show)

dateTimeFormatsSchema :: FC.Fleece schema => schema DateTimeFormats
dateTimeFormatsSchema =
  FC.object $
    FC.constructor DateTimeFormats
      #+ FC.optional "zonedTimeField" zonedTimeField ZonedTimeField.zonedTimeFieldSchema
      #+ FC.optional "defaultTimeField" defaultTimeField DefaultTimeField.defaultTimeFieldSchema
      #+ FC.optional "localTimeField" localTimeField LocalTimeField.localTimeFieldSchema
      #+ FC.optional "utcTimeField" utcTimeField UtcTimeField.utcTimeFieldSchema