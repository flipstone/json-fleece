{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetFull.CardWidth
  ( CardWidth(..)
  , cardWidthSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Double, Eq, Show)

newtype CardWidth = CardWidth Double
  deriving (Show, Eq)

cardWidthSchema :: FC.Fleece schema => schema CardWidth
cardWidthSchema =
  FC.coerceSchema FC.double