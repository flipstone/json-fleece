{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardBase
  ( TradingCardBase(..)
  , tradingCardBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Integer, Maybe, Show)
import StarTrek.TradingCardDeckHeader (TradingCardDeckHeader, tradingCardDeckHeaderSchema)
import StarTrek.TradingCardSetHeader (TradingCardSetHeader, tradingCardSetHeaderSchema)

data TradingCardBase = TradingCardBase
  { number :: Maybe Text -- ^ Trading card number
  , tradingCardSet :: Maybe TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , name :: Text -- ^ Trading card name
  , releaseYear :: Maybe Integer -- ^ Release year, if set was releases over multiple years
  , productionRun :: Maybe Integer -- ^ Production run, if different from trading card set production run
  , uid :: Text -- ^ Trading card unique ID
  , tradingCardDeck :: Maybe TradingCardDeckHeader -- ^ Header trading card deck, embedded in other objects
  }
  deriving (Eq, Show)

tradingCardBaseSchema :: FC.Fleece schema => schema TradingCardBase
tradingCardBaseSchema =
  FC.object $
    FC.constructor TradingCardBase
      #+ FC.optional "number" number FC.text
      #+ FC.optional "tradingCardSet" tradingCardSet tradingCardSetHeaderSchema
      #+ FC.required "name" name FC.text
      #+ FC.optional "releaseYear" releaseYear FC.integer
      #+ FC.optional "productionRun" productionRun FC.integer
      #+ FC.required "uid" uid FC.text
      #+ FC.optional "tradingCardDeck" tradingCardDeck tradingCardDeckHeaderSchema