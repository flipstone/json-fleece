{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}

module StarTrek.Operations.TradingCard
  ( TradingCard(..)
  , route
  ) where

import Beeline.Routing ((/-))
import qualified Beeline.Routing as R
import Prelude (($), Eq, Show)

data TradingCard = TradingCard
  deriving (Eq, Show)

route :: R.Router r => r TradingCard
route =
  R.get $
    R.make TradingCard
      /- "tradingCard"