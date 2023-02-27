{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialBase.Explosive
  ( Explosive(..)
  , explosiveSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype Explosive = Explosive Bool
  deriving (Show, Eq)

explosiveSchema :: FC.Fleece schema => schema Explosive
explosiveSchema =
  FC.coerceSchema FC.boolean