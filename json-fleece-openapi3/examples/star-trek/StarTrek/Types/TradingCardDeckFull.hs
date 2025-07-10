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
  { frequency :: Maybe Frequency.Frequency -- ^ Frequency with which this deck occur in it's set
  , name :: Name.Name -- ^ Trading card deck name
  , tradingCardSet :: Maybe TradingCardSetHeader.TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , tradingCards :: Maybe [TradingCardBase.TradingCardBase] -- ^ Base trading card, returned in search results
  , uid :: Uid.Uid -- ^ Trading card deck unique ID
  }
  deriving (Eq, Show)

tradingCardDeckFullSchema :: FC.Fleece schema => schema TradingCardDeckFull
tradingCardDeckFullSchema =
  FC.object $
    FC.constructor TradingCardDeckFull
      #+ FC.optional "frequency" frequency Frequency.frequencySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "tradingCardSet" tradingCardSet TradingCardSetHeader.tradingCardSetHeaderSchema
      #+ FC.optional "tradingCards" tradingCards (FC.list TradingCardBase.tradingCardBaseSchema)
      #+ FC.required "uid" uid Uid.uidSchema