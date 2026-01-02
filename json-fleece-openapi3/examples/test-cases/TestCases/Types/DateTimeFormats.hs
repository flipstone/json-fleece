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
import qualified TestCases.Types.DateTimeFormats.ZonedTimeInUnionField as ZonedTimeInUnionField

data DateTimeFormats = DateTimeFormats
  { customLocalTimeField :: Maybe CustomLocalTimeField.CustomLocalTimeField
  , customUtcTimeField :: Maybe CustomUtcTimeField.CustomUtcTimeField
  , customZonedTimeField :: Maybe CustomZonedTimeField.CustomZonedTimeField
  , defaultTimeField :: Maybe DefaultTimeField.DefaultTimeField
  , localTimeField :: Maybe LocalTimeField.LocalTimeField
  , utcTimeField :: Maybe UtcTimeField.UtcTimeField
  , zonedTimeField :: Maybe ZonedTimeField.ZonedTimeField
  , zonedTimeInUnionField :: Maybe ZonedTimeInUnionField.ZonedTimeInUnionField
  }
  deriving (Show)

dateTimeFormatsSchema :: FC.Fleece t => FC.Schema t DateTimeFormats
dateTimeFormatsSchema =
  FC.object $
    FC.constructor DateTimeFormats
      #+ FC.optional "customLocalTimeField" customLocalTimeField CustomLocalTimeField.customLocalTimeFieldSchema
      #+ FC.optional "customUtcTimeField" customUtcTimeField CustomUtcTimeField.customUtcTimeFieldSchema
      #+ FC.optional "customZonedTimeField" customZonedTimeField CustomZonedTimeField.customZonedTimeFieldSchema
      #+ FC.optional "defaultTimeField" defaultTimeField DefaultTimeField.defaultTimeFieldSchema
      #+ FC.optional "localTimeField" localTimeField LocalTimeField.localTimeFieldSchema
      #+ FC.optional "utcTimeField" utcTimeField UtcTimeField.utcTimeFieldSchema
      #+ FC.optional "zonedTimeField" zonedTimeField ZonedTimeField.zonedTimeFieldSchema
      #+ FC.optional "zonedTimeInUnionField" zonedTimeInUnionField ZonedTimeInUnionField.zonedTimeInUnionFieldSchema