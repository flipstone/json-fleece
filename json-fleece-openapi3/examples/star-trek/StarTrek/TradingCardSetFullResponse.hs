{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetFullResponse
  ( TradingCardSetFullResponse(..)
  , tradingCardSetFullResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TradingCardSetFull (TradingCardSetFull, tradingCardSetFullSchema)

data TradingCardSetFullResponse = TradingCardSetFullResponse
  { tradingCardSet :: Maybe TradingCardSetFull -- ^ Full trading card set, returned when queried using UID
  }
  deriving (Eq, Show)

tradingCardSetFullResponseSchema :: FC.Fleece schema => schema TradingCardSetFullResponse
tradingCardSetFullResponseSchema =
  FC.object $
    FC.constructor TradingCardSetFullResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "tradingCardSet" tradingCardSet tradingCardSetFullSchema