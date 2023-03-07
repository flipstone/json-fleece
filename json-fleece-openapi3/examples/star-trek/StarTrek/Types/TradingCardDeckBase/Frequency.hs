{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardDeckBase.Frequency
  ( Frequency(..)
  , frequencySchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Frequency = Frequency T.Text
  deriving (Show, Eq)

frequencySchema :: FC.Fleece schema => schema Frequency
frequencySchema =
  FC.coerceSchema FC.text