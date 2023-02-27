{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardFull.Number
  ( Number(..)
  , numberSchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Number = Number Text
  deriving (Show, Eq)

numberSchema :: FC.Fleece schema => schema Number
numberSchema =
  FC.coerceSchema FC.text