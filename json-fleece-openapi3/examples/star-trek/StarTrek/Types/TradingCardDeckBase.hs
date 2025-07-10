{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardDeckBase
  ( TradingCardDeckBase(..)
  , tradingCardDeckBaseSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified StarTrek.Types.TradingCardDeckBase.Frequency as Frequency
import qualified StarTrek.Types.TradingCardDeckBase.Name as Name
import qualified StarTrek.Types.TradingCardDeckBase.Uid as Uid
import qualified StarTrek.Types.TradingCardSetHeader as TradingCardSetHeader

data TradingCardDeckBase = TradingCardDeckBase
  { frequency :: Maybe Frequency.Frequency -- ^ Frequency with which this deck occur in it's set
  , name :: Name.Name -- ^ Trading card deck name
  , tradingCardSet :: Maybe TradingCardSetHeader.TradingCardSetHeader -- ^ Header trading card set, embedded in other objects
  , uid :: Uid.Uid -- ^ Trading card deck unique ID
  }
  deriving (Eq, Show)

tradingCardDeckBaseSchema :: FC.Fleece schema => schema TradingCardDeckBase
tradingCardDeckBaseSchema =
  FC.object $
    FC.constructor TradingCardDeckBase
      #+ FC.optional "frequency" frequency Frequency.frequencySchema
      #+ FC.required "name" name Name.nameSchema
      #+ FC.optional "tradingCardSet" tradingCardSet TradingCardSetHeader.tradingCardSetHeaderSchema
      #+ FC.required "uid" uid Uid.uidSchema