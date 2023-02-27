{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.LocationFull.BajoranSettlement
  ( BajoranSettlement(..)
  , bajoranSettlementSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BajoranSettlement = BajoranSettlement Bool
  deriving (Show, Eq)

bajoranSettlementSchema :: FC.Fleece schema => schema BajoranSettlement
bajoranSettlementSchema =
  FC.coerceSchema FC.boolean