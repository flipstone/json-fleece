{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.FoodFull.HerbOrSpice
  ( HerbOrSpice(..)
  , herbOrSpiceSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype HerbOrSpice = HerbOrSpice Bool
  deriving (Show, Eq)

herbOrSpiceSchema :: FC.Fleece schema => schema HerbOrSpice
herbOrSpiceSchema =
  FC.coerceSchema FC.boolean