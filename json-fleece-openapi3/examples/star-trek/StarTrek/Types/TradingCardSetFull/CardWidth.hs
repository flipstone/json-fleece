{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetFull.CardWidth
  ( CardWidth(..)
  , cardWidthSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Double, Eq, Show)

newtype CardWidth = CardWidth Double
  deriving (Show, Eq)

cardWidthSchema :: FC.Fleece t => FC.Schema t CardWidth
cardWidthSchema =
  FC.coerceSchema FC.double