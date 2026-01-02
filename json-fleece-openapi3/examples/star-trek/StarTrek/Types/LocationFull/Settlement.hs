{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationFull.Settlement
  ( Settlement(..)
  , settlementSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Settlement = Settlement Bool
  deriving (Show, Eq)

settlementSchema :: FC.Fleece t => FC.Schema t Settlement
settlementSchema =
  FC.coerceSchema FC.boolean