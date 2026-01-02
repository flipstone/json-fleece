{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardFullResponse
  ( TradingCardFullResponse(..)
  , tradingCardFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.TradingCardFull as TradingCardFull

data TradingCardFullResponse = TradingCardFullResponse
  { tradingCard :: Maybe TradingCardFull.TradingCardFull -- ^ Full trading card, returned when queried using UID
  }
  deriving (Eq, Show)

tradingCardFullResponseSchema :: FC.Fleece t => FC.Schema t TradingCardFullResponse
tradingCardFullResponseSchema =
  FC.object $
    FC.constructor TradingCardFullResponse
      #+ FC.optional "tradingCard" tradingCard TradingCardFull.tradingCardFullSchema