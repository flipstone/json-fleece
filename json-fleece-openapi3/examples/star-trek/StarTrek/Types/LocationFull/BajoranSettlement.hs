{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.BajoranSettlement
  ( BajoranSettlement(..)
  , bajoranSettlementSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype BajoranSettlement = BajoranSettlement Bool
  deriving (Show, Eq)

bajoranSettlementSchema :: FC.Fleece t => FC.Schema t BajoranSettlement
bajoranSettlementSchema =
  FC.coerceSchema FC.boolean