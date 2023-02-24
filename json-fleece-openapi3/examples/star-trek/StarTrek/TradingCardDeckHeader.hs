{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardDeckHeader
  ( TradingCardDeckHeader(..)
  , tradingCardDeckHeaderSchema
  ) where

import qualified Fleece.Core as FC
import Data.Text (Text)
import Fleece.Core ((#+))
import Prelude (($), Eq, Show)

data TradingCardDeckHeader = TradingCardDeckHeader
  { name :: Text -- ^ Trading card deck name
  , uid :: Text -- ^ Trading card deck unique ID
  }
  deriving (Eq, Show)

tradingCardDeckHeaderSchema :: FC.Fleece schema => schema TradingCardDeckHeader
tradingCardDeckHeaderSchema =
  FC.object $
    FC.constructor TradingCardDeckHeader
      #+ FC.required "name" name FC.text
      #+ FC.required "uid" uid FC.text