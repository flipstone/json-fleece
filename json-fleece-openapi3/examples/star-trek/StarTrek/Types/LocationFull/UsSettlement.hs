{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.UsSettlement
  ( UsSettlement(..)
  , usSettlementSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype UsSettlement = UsSettlement Bool
  deriving (Show, Eq)

usSettlementSchema :: FC.Fleece schema => schema UsSettlement
usSettlementSchema =
  FC.coerceSchema FC.boolean