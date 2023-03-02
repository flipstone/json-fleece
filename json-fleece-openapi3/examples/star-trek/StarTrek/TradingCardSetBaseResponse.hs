{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetBaseResponse
  ( TradingCardSetBaseResponse(..)
  , tradingCardSetBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.ResponsePage as ResponsePage
import qualified StarTrek.ResponseSort as ResponseSort
import qualified StarTrek.TradingCardSetBase as TradingCardSetBase

data TradingCardSetBaseResponse = TradingCardSetBaseResponse
  { sort :: Maybe ResponseSort.ResponseSort -- ^ Response sort
  , tradingCardSets :: Maybe [TradingCardSetBase.TradingCardSetBase] -- ^ Base trading card set, returned in search results
  , page :: Maybe ResponsePage.ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

tradingCardSetBaseResponseSchema :: FC.Fleece schema => schema TradingCardSetBaseResponse
tradingCardSetBaseResponseSchema =
  FC.object $
    FC.constructor TradingCardSetBaseResponse
      #+ FC.optional "sort" sort ResponseSort.responseSortSchema
      #+ FC.optional "tradingCardSets" tradingCardSets (FC.list TradingCardSetBase.tradingCardSetBaseSchema)
      #+ FC.optional "page" page ResponsePage.responsePageSchema