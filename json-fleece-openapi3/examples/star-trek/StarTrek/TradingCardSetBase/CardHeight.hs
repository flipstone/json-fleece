{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetBase.CardHeight
  ( CardHeight(..)
  , cardHeightSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Double, Eq, Show)

newtype CardHeight = CardHeight Double
  deriving (Show, Eq)

cardHeightSchema :: FC.Fleece schema => schema CardHeight
cardHeightSchema =
  FC.coerceSchema FC.double