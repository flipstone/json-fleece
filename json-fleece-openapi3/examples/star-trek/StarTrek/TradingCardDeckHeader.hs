{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckHeader
  ( TradingCardDeckHeader(..)
  , tradingCardDeckHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.TradingCardDeckHeader.Name (Name, nameSchema)
import StarTrek.TradingCardDeckHeader.Uid (Uid, uidSchema)

data TradingCardDeckHeader = TradingCardDeckHeader
  { name :: Name -- ^ Trading card deck name
  , uid :: Uid -- ^ Trading card deck unique ID
  }
  deriving (Eq, Show)

tradingCardDeckHeaderSchema :: FC.Fleece schema => schema TradingCardDeckHeader
tradingCardDeckHeaderSchema =
  FC.object $
    FC.constructor TradingCardDeckHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema