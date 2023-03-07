{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialFull.PoisonousSubstance
  ( PoisonousSubstance(..)
  , poisonousSubstanceSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PoisonousSubstance = PoisonousSubstance Bool
  deriving (Show, Eq)

poisonousSubstanceSchema :: FC.Fleece schema => schema PoisonousSubstance
poisonousSubstanceSchema =
  FC.coerceSchema FC.boolean