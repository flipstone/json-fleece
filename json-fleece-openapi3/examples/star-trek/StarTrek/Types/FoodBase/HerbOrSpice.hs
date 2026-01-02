{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.FoodBase.HerbOrSpice
  ( HerbOrSpice(..)
  , herbOrSpiceSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HerbOrSpice = HerbOrSpice Bool
  deriving (Show, Eq)

herbOrSpiceSchema :: FC.Fleece t => FC.Schema t HerbOrSpice
herbOrSpiceSchema =
  FC.coerceSchema FC.boolean