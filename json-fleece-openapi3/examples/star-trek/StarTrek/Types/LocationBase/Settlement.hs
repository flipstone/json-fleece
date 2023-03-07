{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.LocationBase.Settlement
  ( Settlement(..)
  , settlementSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Settlement = Settlement Bool
  deriving (Show, Eq)

settlementSchema :: FC.Fleece schema => schema Settlement
settlementSchema =
  FC.coerceSchema FC.boolean