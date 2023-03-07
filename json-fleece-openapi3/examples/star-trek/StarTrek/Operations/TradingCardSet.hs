{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.TradingCardSet
  ( TradingCardSet(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data TradingCardSet = TradingCardSet
  deriving (Eq, Show)

route :: R.Router r => r TradingCardSet
route =
  R.get $
    R.make TradingCardSet
      /- "tradingCardSet"