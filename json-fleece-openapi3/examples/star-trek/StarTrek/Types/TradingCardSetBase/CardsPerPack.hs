{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetBase.CardsPerPack
  ( CardsPerPack(..)
  , cardsPerPackSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype CardsPerPack = CardsPerPack Integer
  deriving (Show, Eq)

cardsPerPackSchema :: FC.Fleece t => FC.Schema t CardsPerPack
cardsPerPackSchema =
  FC.coerceSchema FC.integer