{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.ElementBase.Transuranium
  ( Transuranium(..)
  , transuraniumSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Transuranium = Transuranium Bool
  deriving (Show, Eq)

transuraniumSchema :: FC.Fleece schema => schema Transuranium
transuraniumSchema =
  FC.coerceSchema FC.boolean