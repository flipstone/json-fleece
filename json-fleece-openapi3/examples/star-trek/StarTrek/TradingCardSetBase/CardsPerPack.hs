{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetBase.CardsPerPack
  ( CardsPerPack(..)
  , cardsPerPackSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype CardsPerPack = CardsPerPack Integer
  deriving (Show, Eq)

cardsPerPackSchema :: FC.Fleece schema => schema CardsPerPack
cardsPerPackSchema =
  FC.coerceSchema FC.integer