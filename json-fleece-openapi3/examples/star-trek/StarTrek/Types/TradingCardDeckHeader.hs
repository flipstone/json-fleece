{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardDeckHeader
  ( TradingCardDeckHeader(..)
  , tradingCardDeckHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.TradingCardDeckHeader.Name as Name
import qualified StarTrek.Types.TradingCardDeckHeader.Uid as Uid

data TradingCardDeckHeader = TradingCardDeckHeader
  { name :: Name.Name -- ^ Trading card deck name
  , uid :: Uid.Uid -- ^ Trading card deck unique ID
  }
  deriving (Eq, Show)

tradingCardDeckHeaderSchema :: FC.Fleece t => FC.Schema t TradingCardDeckHeader
tradingCardDeckHeaderSchema =
  FC.object $
    FC.constructor TradingCardDeckHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema