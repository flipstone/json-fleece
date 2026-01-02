{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetFullResponse
  ( TradingCardSetFullResponse(..)
  , tradingCardSetFullResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.TradingCardSetFull as TradingCardSetFull

data TradingCardSetFullResponse = TradingCardSetFullResponse
  { tradingCardSet :: Maybe TradingCardSetFull.TradingCardSetFull -- ^ Full trading card set, returned when queried using UID
  }
  deriving (Eq, Show)

tradingCardSetFullResponseSchema :: FC.Fleece t => FC.Schema t TradingCardSetFullResponse
tradingCardSetFullResponseSchema =
  FC.object $
    FC.constructor TradingCardSetFullResponse
      #+ FC.optional "tradingCardSet" tradingCardSet TradingCardSetFull.tradingCardSetFullSchema