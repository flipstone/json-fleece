{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DateTimeFormats
  ( DateTimeFormats(..)
  , dateTimeFormatsSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Maybe, Show)
import qualified TestCases.Types.DateTimeFormats.CustomLocalTimeField as CustomLocalTimeField
import qualified TestCases.Types.DateTimeFormats.CustomUtcTimeField as CustomUtcTimeField
import qualified TestCases.Types.DateTimeFormats.CustomZonedTimeField as CustomZonedTimeField
import qualified TestCases.Types.DateTimeFormats.DefaultTimeField as DefaultTimeField
import qualified TestCases.Types.DateTimeFormats.LocalTimeField as LocalTimeField
import qualified TestCases.Types.DateTimeFormats.UtcTimeField as UtcTimeField
import qualified TestCases.Types.DateTimeFormats.ZonedTimeField as ZonedTimeField

data DateTimeFormats = DateTimeFormats
  { zonedTimeField :: Maybe ZonedTimeField.ZonedTimeField
  , defaultTimeField :: Maybe DefaultTimeField.DefaultTimeField
  , localTimeField :: Maybe LocalTimeField.LocalTimeField
  , customLocalTimeField :: Maybe CustomLocalTimeField.CustomLocalTimeField
  , utcTimeField :: Maybe UtcTimeField.UtcTimeField
  , customZonedTimeField :: Maybe CustomZonedTimeField.CustomZonedTimeField
  , customUtcTimeField :: Maybe CustomUtcTimeField.CustomUtcTimeField
  }
  deriving (Show)

dateTimeFormatsSchema :: FC.Fleece schema => schema DateTimeFormats
dateTimeFormatsSchema =
  FC.object $
    FC.constructor DateTimeFormats
      #+ FC.optional "zonedTimeField" zonedTimeField ZonedTimeField.zonedTimeFieldSchema
      #+ FC.optional "defaultTimeField" defaultTimeField DefaultTimeField.defaultTimeFieldSchema
      #+ FC.optional "localTimeField" localTimeField LocalTimeField.localTimeFieldSchema
      #+ FC.optional "customLocalTimeField" customLocalTimeField CustomLocalTimeField.customLocalTimeFieldSchema
      #+ FC.optional "utcTimeField" utcTimeField UtcTimeField.utcTimeFieldSchema
      #+ FC.optional "customZonedTimeField" customZonedTimeField CustomZonedTimeField.customZonedTimeFieldSchema
      #+ FC.optional "customUtcTimeField" customUtcTimeField CustomUtcTimeField.customUtcTimeFieldSchema