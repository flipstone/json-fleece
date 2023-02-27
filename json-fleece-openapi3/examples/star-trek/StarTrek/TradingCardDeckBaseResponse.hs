{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckBaseResponse
  ( TradingCardDeckBaseResponse(..)
  , tradingCardDeckBaseResponseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
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
      #+ FC.optional "tradingCardDecks" tradingCardDecks (FC.list tradingCardDeckBaseSchema)
      #+ FC.optional "sort" sort responseSortSchema
      #+ FC.optional "page" page responsePageSchema