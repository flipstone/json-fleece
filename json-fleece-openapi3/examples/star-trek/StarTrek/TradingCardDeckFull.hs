{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckFull
  ( TradingCardDeckFull(..)
  , tradingCardDeckFullSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TradingCardBase (TradingCardBase, tradingCardBaseSchema)
import StarTrek.TradingCardSetHeader (TradingCardSetHeader, tradingCardSetHeaderSchema)

data TradingCardDeckFull = TradingCardDeckFull
  { tradingCardSet :: Maybe TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , name :: Text -- ^ Trading card deck name
  , frequency :: Maybe Text -- ^ Frequency with which this deck occur in it's set
  , uid :: Text -- ^ Trading card deck unique ID
  , tradingCards :: Maybe [TradingCardBase] -- ^ Trading cards in this deck
  }
  deriving (Eq, Show)

tradingCardDeckFullSchema :: FC.Fleece schema => schema TradingCardDeckFull
tradingCardDeckFullSchema =
  FC.object $
    FC.constructor TradingCardDeckFull
      #+ FC.optionalField FC.OmitKey_DelegateNull "tradingCardSet" tradingCardSet tradingCardSetHeaderSchema
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "frequency" frequency FC.text
      #+ FC.required "uid" uid FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "tradingCards" tradingCards (FC.list tradingCardBaseSchema)