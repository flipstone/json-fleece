{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodBase.Sauce
  ( Sauce(..)
  , sauceSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Sauce = Sauce Bool
  deriving (Show, Eq)

sauceSchema :: FC.Fleece t => FC.Schema t Sauce
sauceSchema =
  FC.coerceSchema FC.boolean