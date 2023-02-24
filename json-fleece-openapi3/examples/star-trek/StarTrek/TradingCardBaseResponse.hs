{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardBaseResponse
  ( TradingCardBaseResponse(..)
  , tradingCardBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.TradingCardBase (TradingCardBase, tradingCardBaseSchema)

data TradingCardBaseResponse = TradingCardBaseResponse
  { tradingCards :: Maybe [TradingCardBase] -- ^ List of trading cards matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

tradingCardBaseResponseSchema :: FC.Fleece schema => schema TradingCardBaseResponse
tradingCardBaseResponseSchema =
  FC.object $
    FC.constructor TradingCardBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "tradingCards" tradingCards (FC.list tradingCardBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema