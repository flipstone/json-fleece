{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetBaseResponse
  ( TradingCardSetBaseResponse(..)
  , tradingCardSetBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.ResponsePage as ResponsePage
import qualified StarTrek.Types.ResponseSort as ResponseSort
import qualified StarTrek.Types.TradingCardSetBase as TradingCardSetBase

data TradingCardSetBaseResponse = TradingCardSetBaseResponse
  { page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  , sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , tradingCardSets :: Maybe [TradingCardSetBase.TradingCardSetBase] -- ^ Base trading card set, returned in search results
  }
  deriving (Eq, Show)

tradingCardSetBaseResponseSchema :: FC.Fleece t => FC.Schema t TradingCardSetBaseResponse
tradingCardSetBaseResponseSchema =
  FC.object $
    FC.constructor TradingCardSetBaseResponse
      #+ FC.optional "page" page ResponsePage.responsePageSchema
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "tradingCardSets" tradingCardSets (FC.list TradingCardSetBase.tradingCardSetBaseSchema)