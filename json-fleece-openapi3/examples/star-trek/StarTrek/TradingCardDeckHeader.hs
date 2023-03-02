{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckHeader
  ( TradingCardDeckHeader(..)
  , tradingCardDeckHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.TradingCardDeckHeader.Name as Name
import qualified StarTrek.TradingCardDeckHeader.Uid as Uid

data TradingCardDeckHeader = TradingCardDeckHeader
  { name :: Name.Name -- ^ Trading card deck name
  , uid :: Uid.Uid -- ^ Trading card deck unique ID
  }
  deriving (Eq, Show)

tradingCardDeckHeaderSchema :: FC.Fleece schema => schema TradingCardDeckHeader
tradingCardDeckHeaderSchema =
  FC.object $
    FC.constructor TradingCardDeckHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema