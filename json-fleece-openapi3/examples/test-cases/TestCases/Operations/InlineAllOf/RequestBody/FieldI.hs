{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Operations.InlineAllOf.RequestBody.FieldI
  ( FieldI(..)
  , fieldISchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified TestCases.Types.InlineAllOf.RequestBody.FieldI.DatetimeType as DatetimeType
import qualified TestCases.Types.InlineAllOf.RequestBody.FieldI.DatetimeValue as DatetimeValue
import qualified TestCases.Types.InlineAllOf.RequestBody.FieldI.Type as Type

data FieldI = FieldI
  { datetimeType :: DatetimeType.DatetimeType -- ^ Type of the datetime
  , datetimeValue :: DatetimeValue.DatetimeValue -- ^ UTC datetime value
  , type_ :: Maybe Type.Type
  }
  deriving (Eq, Show)

fieldISchema :: FC.Fleece t => FC.Schema t FieldI
fieldISchema =
  FC.object $
    FC.constructor FieldI
      #+ FC.required "datetime_type" datetimeType DatetimeType.datetimeTypeSchema
      #+ FC.required "datetime_value" datetimeValue DatetimeValue.datetimeValueSchema
      #+ FC.optional "type" type_ Type.typeSchema