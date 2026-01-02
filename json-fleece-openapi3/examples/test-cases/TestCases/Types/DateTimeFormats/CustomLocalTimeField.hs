{-# LANGUAGE NoImplicitPrelude #-}

module TestCases.Types.DateTimeFormats.CustomLocalTimeField
  ( CustomLocalTimeField(..)
  , customLocalTimeFieldSchema
  ) where

import qualified Data.Time as Time
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype CustomLocalTimeField = CustomLocalTimeField Time.LocalTime
  deriving (Show, Eq)

customLocalTimeFieldSchema :: FC.Fleece t => FC.Schema t CustomLocalTimeField
customLocalTimeFieldSchema =
  FC.coerceSchema (FC.localTimeWithFormat "local")