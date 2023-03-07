{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardBase.Number
  ( Number(..)
  , numberSchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Number = Number T.Text
  deriving (Show, Eq)

numberSchema :: FC.Fleece schema => schema Number
numberSchema =
  FC.coerceSchema FC.text