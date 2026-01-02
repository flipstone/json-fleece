{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardHeader
  ( TradingCardHeader(..)
  , tradingCardHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.TradingCardHeader.Name as Name
import qualified StarTrek.Types.TradingCardHeader.Uid as Uid

data TradingCardHeader = TradingCardHeader
  { name :: Name.Name -- ^ Trading card name
  , uid :: Uid.Uid -- ^ Trading card unique ID
  }
  deriving (Eq, Show)

tradingCardHeaderSchema :: FC.Fleece t => FC.Schema t TradingCardHeader
tradingCardHeaderSchema =
  FC.object $
    FC.constructor TradingCardHeader
      #+ FC.required "name" name Name.nameSchema
      #+ FC.required "uid" uid Uid.uidSchema