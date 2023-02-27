{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardFull
  ( TradingCardFull(..)
  , tradingCardFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.TradingCardDeckBase (TradingCardDeckBase, tradingCardDeckBaseSchema)
import StarTrek.TradingCardSetBase (TradingCardSetBase, tradingCardSetBaseSchema)

data TradingCardFull = TradingCardFull
  { number :: Maybe Text -- ^ Trading card number
  , tradingCardSet :: Maybe TradingCardSetBase -- ^ Base trading card set, returned in search results
  , name :: Text -- ^ Trading card name
  , releaseYear :: Maybe Integer -- ^ Release year, if set was releases over multiple years
  , productionRun :: Maybe Integer -- ^ Production run, if different from trading card set production run
  , uid :: Text -- ^ Trading card unique ID
  , tradingCardDeck :: Maybe TradingCardDeckBase -- ^ Base trading card deck, returned in search results
  }
  deriving (Eq, Show)

tradingCardFullSchema :: FC.Fleece schema => schema TradingCardFull
tradingCardFullSchema =
  FC.object $
    FC.constructor TradingCardFull
      #+ FC.optional "number" number FC.text
      #+ FC.optional "tradingCardSet" tradingCardSet tradingCardSetBaseSchema
      #+ FC.required "name" name FC.text
      #+ FC.optional "releaseYear" releaseYear FC.integer
      #+ FC.optional "productionRun" productionRun FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "tradingCardDeck" tradingCardDeck tradingCardDeckBaseSchema