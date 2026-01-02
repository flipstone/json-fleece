{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.TradingCardSetBase.PacksPerBox
  ( PacksPerBox(..)
  , packsPerBoxSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PacksPerBox = PacksPerBox Integer
  deriving (Show, Eq)

packsPerBoxSchema :: FC.Fleece t => FC.Schema t PacksPerBox
packsPerBoxSchema =
  FC.coerceSchema FC.integer