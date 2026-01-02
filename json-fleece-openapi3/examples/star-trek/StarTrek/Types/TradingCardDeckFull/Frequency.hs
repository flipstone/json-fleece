{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardDeckFull.Frequency
  ( Frequency(..)
  , frequencySchema
  ) where

import qualified Data.Text as T
import qualified Fleece.Core as FC
import Prelude (Eq, Show)

newtype Frequency = Frequency T.Text
  deriving (Show, Eq)

frequencySchema :: FC.Fleece t => FC.Schema t Frequency
frequencySchema =
  FC.coerceSchema FC.text