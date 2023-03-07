{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodFull.AlcoholicBeverage
  ( AlcoholicBeverage(..)
  , alcoholicBeverageSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AlcoholicBeverage = AlcoholicBeverage Bool
  deriving (Show, Eq)

alcoholicBeverageSchema :: FC.Fleece schema => schema AlcoholicBeverage
alcoholicBeverageSchema =
  FC.coerceSchema FC.boolean