{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.PriceEstimate.CurrencyCode
  ( CurrencyCode(..)
  , currencyCodeSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype CurrencyCode = CurrencyCode T.Text
  deriving (Show, Eq)

currencyCodeSchema :: FC.Fleece t => FC.Schema t CurrencyCode
currencyCodeSchema =
  FC.coerceSchema FC.text