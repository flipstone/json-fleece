{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetBaseResponse
  ( TradingCardSetBaseResponse(..)
  , tradingCardSetBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
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
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "tradingCardSets" tradingCardSets (FC.list tradingCardSetBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema