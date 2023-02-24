{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckBase
  ( TradingCardDeckBase(..)
  , tradingCardDeckBaseSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TradingCardSetHeader (TradingCardSetHeader, tradingCardSetHeaderSchema)

data TradingCardDeckBase = TradingCardDeckBase
  { tradingCardSet :: Maybe TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , name :: Text -- ^ Trading card deck name
  , frequency :: Maybe Text -- ^ Frequency with which this deck occur in it's set
  , uid :: Text -- ^ Trading card deck unique ID
  }
  deriving (Eq, Show)

tradingCardDeckBaseSchema :: FC.Fleece schema => schema TradingCardDeckBase
tradingCardDeckBaseSchema =
  FC.object $
    FC.constructor TradingCardDeckBase
      #+ FC.optionalField FC.OmitKey_DelegateNull "tradingCardSet" tradingCardSet tradingCardSetHeaderSchema
      #+ FC.required "name" name FC.text
      #+ FC.optionalField FC.OmitKey_DelegateNull "frequency" frequency FC.text
      #+ FC.required "uid" uid FC.text