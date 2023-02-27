{-# LANGUAGE NoImplicitPrelude #-}

module Uber.PriceEstimate.CurrencyCode
  ( CurrencyCode(..)
  , currencyCodeSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype CurrencyCode = CurrencyCode Text
  deriving (Show, Eq)

currencyCodeSchema :: FC.Fleece schema => schema CurrencyCode
currencyCodeSchema =
  FC.coerceSchema FC.text