{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckFull
  ( TradingCardDeckFull(..)
  , tradingCardDeckFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import StarTrek.TradingCardBase (TradingCardBase, tradingCardBaseSchema)
import StarTrek.TradingCardDeckFull.Frequency (Frequency, frequencySchema)
import StarTrek.TradingCardDeckFull.Name (Name, nameSchema)
import StarTrek.TradingCardDeckFull.Uid (Uid, uidSchema)
import StarTrek.TradingCardSetHeader (TradingCardSetHeader, tradingCardSetHeaderSchema)

data TradingCardDeckFull = TradingCardDeckFull
  { tradingCardSet :: Maybe TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , name :: Name -- ^ Trading card deck name
  , frequency :: Maybe Frequency -- ^ Frequency with which this deck occur in it's set
  , uid :: Uid -- ^ Trading card deck unique ID
  , tradingCards :: Maybe [TradingCardBase] -- ^ Base trading card, returned in search results
  }
  deriving (Eq, Show)

tradingCardDeckFullSchema :: FC.Fleece schema => schema TradingCardDeckFull
tradingCardDeckFullSchema =
  FC.object $
    FC.constructor TradingCardDeckFull
      #+ FC.optional "tradingCardSet" tradingCardSet tradingCardSetHeaderSchema
      #+ FC.required "name" name nameSchema
      #+ FC.optional "frequency" frequency frequencySchema
      #+ FC.required "uid" uid uidSchema
      #+ FC.optional "tradingCards" tradingCards (FC.list tradingCardBaseSchema)