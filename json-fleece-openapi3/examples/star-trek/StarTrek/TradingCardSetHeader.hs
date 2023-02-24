{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetHeader
  ( TradingCardSetHeader(..)
  , tradingCardSetHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data TradingCardSetHeader = TradingCardSetHeader
  { name :: Text -- ^ Trading card set name
  , uid :: Text -- ^ Trading card set unique ID
  }
  deriving (Eq, Show)

tradingCardSetHeaderSchema :: FC.Fleece schema => schema TradingCardSetHeader
tradingCardSetHeaderSchema =
  FC.object $
    FC.constructor TradingCardSetHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text