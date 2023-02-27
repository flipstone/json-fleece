{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.TradingCardSetFull.PacksPerBox
  ( PacksPerBox(..)
  , packsPerBoxSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Eq, Integer, Show)

newtype PacksPerBox = PacksPerBox Integer
  deriving (Show, Eq)

packsPerBoxSchema :: FC.Fleece schema => schema PacksPerBox
packsPerBoxSchema =
  FC.coerceSchema FC.integer