{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckBase
  ( TradingCardDeckBase(..)
  , tradingCardDeckBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TradingCardDeckBase.Frequency (Frequency, frequencySchema)
import StarTrek.TradingCardDeckBase.Name (Name, nameSchema)
import StarTrek.TradingCardDeckBase.Uid (Uid, uidSchema)
import StarTrek.TradingCardSetHeader (TradingCardSetHeader, tradingCardSetHeaderSchema)

data TradingCardDeckBase = TradingCardDeckBase
  { tradingCardSet :: Maybe TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , name :: Name -- ^ Trading card deck name
  , frequency :: Maybe Frequency -- ^ Frequency with which this deck occur in it's set
  , uid :: Uid -- ^ Trading card deck unique ID
  }
  deriving (Eq, Show)

tradingCardDeckBaseSchema :: FC.Fleece schema => schema TradingCardDeckBase
tradingCardDeckBaseSchema =
  FC.object $
    FC.constructor TradingCardDeckBase
      #+ FC.optional "tradingCardSet" tradingCardSet tradingCardSetHeaderSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "frequency" frequency frequencySchema
      #+ FC.required "uid" uid uidSchema