{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckBaseResponse
  ( TradingCardDeckBaseResponse(..)
  , tradingCardDeckBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort
import qualified StarTrek.TradingCardDeckBase as TradingCardDeckBase

data TradingCardDeckBaseResponse = TradingCardDeckBaseResponse
  { tradingCardDecks :: Maybe [TradingCardDeckBase.TradingCardDeckBase] -- ^ Base trading card deck, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

tradingCardDeckBaseResponseSchema :: FC.Fleece schema => schema TradingCardDeckBaseResponse
tradingCardDeckBaseResponseSchema =
  FC.object $
    FC.constructor TradingCardDeckBaseResponse
      #+ FC.optional "tradingCardDecks" tradingCardDecks (FC.list TradingCardDeckBase.tradingCardDeckBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "page" page ResponsePage.responsePageSchema