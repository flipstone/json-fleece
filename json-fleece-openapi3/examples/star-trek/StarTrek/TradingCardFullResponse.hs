{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardFullResponse
  ( TradingCardFullResponse(..)
  , tradingCardFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TradingCardFull (TradingCardFull, tradingCardFullSchema)

data TradingCardFullResponse = TradingCardFullResponse
  { tradingCard :: Maybe TradingCardFull -- ^ Full trading card, returned when queried using UID
  }
  deriving (Eq, Show)

tradingCardFullResponseSchema :: FC.Fleece schema => schema TradingCardFullResponse
tradingCardFullResponseSchema =
  FC.object $
    FC.constructor TradingCardFullResponse
      #+ FC.optional "tradingCard" tradingCard tradingCardFullSchema