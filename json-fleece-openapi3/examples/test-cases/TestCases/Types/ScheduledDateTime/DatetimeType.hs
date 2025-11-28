{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.ScheduledDateTime.DatetimeType
  ( DatetimeType(..)
  , datetimeTypeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype DatetimeType = DatetimeType T.Text
  deriving (Show, Eq)

datetimeTypeSchema :: FC.Fleece schema => schema DatetimeType
datetimeTypeSchema =
  FC.coerceSchema FC.text