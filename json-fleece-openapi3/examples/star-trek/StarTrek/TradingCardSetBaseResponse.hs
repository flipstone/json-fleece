{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetBaseResponse
  ( TradingCardSetBaseResponse(..)
  , tradingCardSetBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.TradingCardSetBase (TradingCardSetBase, tradingCardSetBaseSchema)

data TradingCardSetBaseResponse = TradingCardSetBaseResponse
  { sort :: Maybe ResponseSort -- ^ Response sort
  , tradingCardSets :: Maybe [TradingCardSetBase] -- ^ List of trading card sets matching given criteria
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

tradingCardSetBaseResponseSchema :: FC.Fleece schema => schema TradingCardSetBaseResponse
tradingCardSetBaseResponseSchema =
  FC.object $
    FC.constructor TradingCardSetBaseResponse
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "tradingCardSets" tradingCardSets (FC.list tradingCardSetBaseSchema)
      #+ FC.optional "page" page responsePageSchema