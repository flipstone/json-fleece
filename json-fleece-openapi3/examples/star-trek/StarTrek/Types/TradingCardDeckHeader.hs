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
  { uid :: Uid.Uid -- ^ Trading card deck unique ID
  , name :: Name.Name -- ^ Trading card deck name
  }
  deriving (Eq, Show)

tradingCardDeckHeaderSchema :: FC.Fleece schema => schema TradingCardDeckHeader
tradingCardDeckHeaderSchema =
  FC.object $
    FC.constructor TradingCardDeckHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema