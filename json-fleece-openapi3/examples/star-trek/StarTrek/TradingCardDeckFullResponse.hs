{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckFullResponse
  ( TradingCardDeckFullResponse(..)
  , tradingCardDeckFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TradingCardDeckFull (TradingCardDeckFull, tradingCardDeckFullSchema)

data TradingCardDeckFullResponse = TradingCardDeckFullResponse
  { tradingCardDeck :: Maybe TradingCardDeckFull -- ^ Full trading card deck, returned when queried using UID
  }
  deriving (Eq, Show)

tradingCardDeckFullResponseSchema :: FC.Fleece schema => schema TradingCardDeckFullResponse
tradingCardDeckFullResponseSchema =
  FC.object $
    FC.constructor TradingCardDeckFullResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "tradingCardDeck" tradingCardDeck tradingCardDeckFullSchema