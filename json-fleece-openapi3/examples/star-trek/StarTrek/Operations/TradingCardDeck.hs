{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.TradingCardDeck
  ( TradingCardDeck(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data TradingCardDeck = TradingCardDeck
  deriving (Eq, Show)

route :: R.Router r => r TradingCardDeck
route =
  R.get $
    R.make TradingCardDeck
      /- "tradingCardDeck"