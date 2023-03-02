{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckFullResponse
  ( TradingCardDeckFullResponse(..)
  , tradingCardDeckFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.TradingCardDeckFull as TradingCardDeckFull

data TradingCardDeckFullResponse = TradingCardDeckFullResponse
  { tradingCardDeck :: Maybe TradingCardDeckFull.TradingCardDeckFull -- ^ Full trading card deck, returned when queried using UID
  }
  deriving (Eq, Show)

tradingCardDeckFullResponseSchema :: FC.Fleece schema => schema TradingCardDeckFullResponse
tradingCardDeckFullResponseSchema =
  FC.object $
    FC.constructor TradingCardDeckFullResponse
      #+ FC.optional "tradingCardDeck" tradingCardDeck TradingCardDeckFull.tradingCardDeckFullSchema