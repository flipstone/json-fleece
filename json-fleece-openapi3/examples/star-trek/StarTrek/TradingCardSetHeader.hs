{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetHeader
  ( TradingCardSetHeader(..)
  , tradingCardSetHeaderSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import StarTrek.TradingCardSetHeader.Name (Name, nameSchema)
import StarTrek.TradingCardSetHeader.Uid (Uid, uidSchema)

data TradingCardSetHeader = TradingCardSetHeader
  { name :: Name -- ^ Trading card set name
  , uid :: Uid -- ^ Trading card set unique ID
  }
  deriving (Eq, Show)

tradingCardSetHeaderSchema :: FC.Fleece schema => schema TradingCardSetHeader
tradingCardSetHeaderSchema =
  FC.object $
    FC.constructor TradingCardSetHeader
      #+ FC.required "name" name nameSchema
      #+ FC.required "uid" uid uidSchema