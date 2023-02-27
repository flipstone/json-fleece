{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardBaseResponse
  ( TradingCardBaseResponse(..)
  , tradingCardBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.TradingCardBase (TradingCardBase, tradingCardBaseSchema)

data TradingCardBaseResponse = TradingCardBaseResponse
  { tradingCards :: Maybe [TradingCardBase] -- ^ Base trading card, returned in search results
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

tradingCardBaseResponseSchema :: FC.Fleece schema => schema TradingCardBaseResponse
tradingCardBaseResponseSchema =
  FC.object $
    FC.constructor TradingCardBaseResponse
      #+ FC.optional "tradingCards" tradingCards (FC.list tradingCardBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema