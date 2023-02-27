{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardHeader
  ( TradingCardHeader(..)
  , tradingCardHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.TradingCardHeader.Name (Name, nameSchema)
import StarTrek.TradingCardHeader.Uid (Uid, uidSchema)

data TradingCardHeader = TradingCardHeader
  { name :: Name -- ^ Trading card name
  , uid :: Uid -- ^ Trading card unique ID
  }
  deriving (Eq, Show)

tradingCardHeaderSchema :: FC.Fleece schema => schema TradingCardHeader
tradingCardHeaderSchema =
  FC.object $
    FC.constructor TradingCardHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema