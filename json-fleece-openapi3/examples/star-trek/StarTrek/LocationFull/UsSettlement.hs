{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull.UsSettlement
  ( UsSettlement(..)
  , usSettlementSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype UsSettlement = UsSettlement Bool
  deriving (Show, Eq)

usSettlementSchema :: FC.Fleece schema => schema UsSettlement
usSettlementSchema =
  FC.coerceSchema FC.boolean