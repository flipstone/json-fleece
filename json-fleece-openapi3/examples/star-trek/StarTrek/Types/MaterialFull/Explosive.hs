{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialFull.Explosive
  ( Explosive(..)
  , explosiveSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Explosive = Explosive Bool
  deriving (Show, Eq)

explosiveSchema :: FC.Fleece t => FC.Schema t Explosive
explosiveSchema =
  FC.coerceSchema FC.boolean