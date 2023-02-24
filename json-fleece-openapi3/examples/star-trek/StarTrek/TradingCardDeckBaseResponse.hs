{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckBaseResponse
  ( TradingCardDeckBaseResponse(..)
  , tradingCardDeckBaseResponseSchema
  ) where

import qualified Fleece.Core as FC
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.ResponsePage (ResponsePage, responsePageSchema)
import StarTrek.ResponseSort (ResponseSort, responseSortSchema)
import StarTrek.TradingCardDeckBase (TradingCardDeckBase, tradingCardDeckBaseSchema)

data TradingCardDeckBaseResponse = TradingCardDeckBaseResponse
  { tradingCardDecks :: Maybe [TradingCardDeckBase] -- ^ List of trading card decks matching given criteria
  , sort :: Maybe ResponseSort -- ^ Response sort
  , page :: Maybe ResponsePage -- ^ Object describing response page
  }
  deriving (Eq, Show)

tradingCardDeckBaseResponseSchema :: FC.Fleece schema => schema TradingCardDeckBaseResponse
tradingCardDeckBaseResponseSchema =
  FC.object $
    FC.constructor TradingCardDeckBaseResponse
      #+ FC.optionalField FC.OmitKey_DelegateNull "tradingCardDecks" tradingCardDecks (FC.list tradingCardDeckBaseSchema)
      #+ FC.optionalField FC.OmitKey_DelegateNull "sort" sort responseSortSchema
      #+ FC.optionalField FC.OmitKey_DelegateNull "page" page responsePageSchema