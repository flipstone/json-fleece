{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetHeader
  ( TradingCardSetHeader(..)
  , tradingCardSetHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified StarTrek.Types.TradingCardSetHeader.Name as Name
import qualified StarTrek.Types.TradingCardSetHeader.Uid as Uid

data TradingCardSetHeader = TradingCardSetHeader
  { uid :: Uid.Uid -- ^ Trading card set unique ID
  , name :: Name.Name -- ^ Trading card set name
  }
  deriving (Eq, Show)

tradingCardSetHeaderSchema :: FC.Fleece schema => schema TradingCardSetHeader
tradingCardSetHeaderSchema =
  FC.object $
    FC.constructor TradingCardSetHeader
      #+ FC.required "uid" uid Uid.uidSchema
      #+ FC.required "name" name Name.nameSchema