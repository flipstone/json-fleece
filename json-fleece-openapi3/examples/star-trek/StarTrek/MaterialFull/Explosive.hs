{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialFull.Explosive
  ( Explosive(..)
  , explosiveSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Explosive = Explosive Bool
  deriving (Show, Eq)

explosiveSchema :: FC.Fleece schema => schema Explosive
explosiveSchema =
  FC.coerceSchema FC.boolean