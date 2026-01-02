{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodBase.AlcoholicBeverage
  ( AlcoholicBeverage(..)
  , alcoholicBeverageSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype AlcoholicBeverage = AlcoholicBeverage Bool
  deriving (Show, Eq)

alcoholicBeverageSchema :: FC.Fleece t => FC.Schema t AlcoholicBeverage
alcoholicBeverageSchema =
  FC.coerceSchema FC.boolean