{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodBase.Sauce
  ( Sauce(..)
  , sauceSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Sauce = Sauce Bool
  deriving (Show, Eq)

sauceSchema :: FC.Fleece schema => schema Sauce
sauceSchema =
  FC.coerceSchema FC.boolean