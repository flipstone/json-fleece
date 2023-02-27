{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckFull.Frequency
  ( Frequency(..)
  , frequencySchema
  ) where

import Data.Text (Text)
import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Frequency = Frequency Text
  deriving (Show, Eq)

frequencySchema :: FC.Fleece schema => schema Frequency
frequencySchema =
  FC.coerceSchema FC.text