{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckBase
  ( TradingCardDeckBase(..)
  , tradingCardDeckBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.TradingCardDeckBase.Frequency as Frequency
import qualified StarTrek.TradingCardDeckBase.Name as Name
import qualified StarTrek.TradingCardDeckBase.Uid as Uid
import qualified StarTrek.TradingCardSetHeader as TradingCardSetHeader

data TradingCardDeckBase = TradingCardDeckBase
  { tradingCardSet :: Maybe TradingCardSetHeader.TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , name :: Name.Name -- ^ Trading card deck name
  , frequency :: Maybe Frequency.Frequency -- ^ Frequency with which this deck occur in it's set
  , uid :: Uid.Uid -- ^ Trading card deck unique ID
  }
  deriving (Eq, Show)

tradingCardDeckBaseSchema :: FC.Fleece schema => schema TradingCardDeckBase
tradingCardDeckBaseSchema =
  FC.object $
    FC.constructor TradingCardDeckBase
      #+ FC.optional "tradingCardSet" tradingCardSet TradingCardSetHeader.tradingCardSetHeaderSchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "frequency" frequency Frequency.frequencySchema
      #+ FC.required "uid" uid Uid.uidSchema