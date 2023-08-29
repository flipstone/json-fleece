{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardDeckFull
  ( TradingCardDeckFull(..)
  , tradingCardDeckFullSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.TradingCardBase as TradingCardBase
import qualified StarTrek.Types.TradingCardDeckFull.Frequency as Frequency
import qualified StarTrek.Types.TradingCardDeckFull.Name as Name
import qualified StarTrek.Types.TradingCardDeckFull.Uid as Uid
import qualified StarTrek.Types.TradingCardSetHeader as TradingCardSetHeader

data TradingCardDeckFull = TradingCardDeckFull
  { tradingCards :: Maybe [TradingCardBase.TradingCardBase] -- ^ Base trading card, returned in search results
  , frequency :: Maybe Frequency.Frequency -- ^ Frequency with which this deck occur in it's set
  , tradingCardSet :: Maybe TradingCardSetHeader.TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , uid :: Uid.Uid -- ^ Trading card deck unique ID
  , name :: Name.Name -- ^ Trading card deck name
  }
  deriving (Eq, Show)

tradingCardDeckFullSchema :: FC.Fleece schema => schema TradingCardDeckFull
tradingCardDeckFullSchema =
  FC.object $
    FC.constructor TradingCardDeckFull
      #+ FC.optional "tradingCards" tradingCards (FC.list TradingCardBase.tradingCardBaseSchema)
      #+ FC.optional "frequency" frequency Frequency.frequencySchema
      #+ FC.optional "tradingCardSet" tradingCardSet TradingCardSetHeader.tradingCardSetHeaderSchema
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema