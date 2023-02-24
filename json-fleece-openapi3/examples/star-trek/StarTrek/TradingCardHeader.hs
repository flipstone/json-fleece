{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardHeader
  ( TradingCardHeader(..)
  , tradingCardHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data TradingCardHeader = TradingCardHeader
  { name :: Text -- ^ Trading card name
  , uid :: Text -- ^ Trading card unique ID
  }
  deriving (Eq, Show)

tradingCardHeaderSchema :: FC.Fleece schema => schema TradingCardHeader
tradingCardHeaderSchema =
  FC.object $
    FC.constructor TradingCardHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text