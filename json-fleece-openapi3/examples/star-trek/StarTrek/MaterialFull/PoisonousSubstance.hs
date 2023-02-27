{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.MaterialFull.PoisonousSubstance
  ( PoisonousSubstance(..)
  , poisonousSubstanceSchema
  ) where

import Fleece.Core ()
import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PoisonousSubstance = PoisonousSubstance Bool
  deriving (Show, Eq)

poisonousSubstanceSchema :: FC.Fleece schema => schema PoisonousSubstance
poisonousSubstanceSchema =
  FC.coerceSchema FC.boolean