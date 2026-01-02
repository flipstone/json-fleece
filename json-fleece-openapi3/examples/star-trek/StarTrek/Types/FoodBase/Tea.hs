{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodBase.Tea
  ( Tea(..)
  , teaSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Tea = Tea Bool
  deriving (Show, Eq)

teaSchema :: FC.Fleece t => FC.Schema t Tea
teaSchema =
  FC.coerceSchema FC.boolean