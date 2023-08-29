{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardDeckBaseResponse
  ( TradingCardDeckBaseResponse(..)
  , tradingCardDeckBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.TradingCardDeckBase as TradingCardDeckBase

data TradingCardDeckBaseResponse = TradingCardDeckBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , tradingCardDecks :: Maybe [TradingCardDeckBase.TradingCardDeckBase] -- ^ Base trading card deck, returned in search results
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  }
  deriving (Eq, Show)

tradingCardDeckBaseResponseSchema :: FC.Fleece schema => schema TradingCardDeckBaseResponse
tradingCardDeckBaseResponseSchema =
  FC.object $
    FC.constructor TradingCardDeckBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "tradingCardDecks" tradingCardDecks (FC.list TradingCardDeckBase.tradingCardDeckBaseSchema)
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema