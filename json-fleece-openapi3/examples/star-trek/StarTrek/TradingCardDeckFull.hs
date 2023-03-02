{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckFull
  ( TradingCardDeckFull(..)
  , tradingCardDeckFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.TradingCardBase as TradingCardBase
import qualified StarTrek.TradingCardDeckFull.Frequency as Frequency
import qualified StarTrek.TradingCardDeckFull.Name as Name
import qualified StarTrek.TradingCardDeckFull.Uid as Uid
import qualified StarTrek.TradingCardSetHeader as TradingCardSetHeader

data TradingCardDeckFull = TradingCardDeckFull
  { tradingCardSet :: Maybe TradingCardSetHeader.TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , name :: Name.Name -- ^ Trading card deck name
  , frequency :: Maybe Frequency.Frequency -- ^ Frequency with which this deck occur in it's set
  , uid :: Uid.Uid -- ^ Trading card deck unique ID
  , tradingCards :: Maybe [TradingCardBase.TradingCardBase] -- ^ Base trading card, returned in search results
  }
  deriving (Eq, Show)

tradingCardDeckFullSchema :: FC.Fleece schema => schema TradingCardDeckFull
tradingCardDeckFullSchema =
  FC.object $
    FC.constructor TradingCardDeckFull
      #+ FC.optional "tradingCardSet" tradingCardSet TradingCardSetHeader.tradingCardSetHeaderSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "frequency" frequency Frequency.frequencySchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.optional "tradingCards" tradingCards (FC.list TradingCardBase.tradingCardBaseSchema)