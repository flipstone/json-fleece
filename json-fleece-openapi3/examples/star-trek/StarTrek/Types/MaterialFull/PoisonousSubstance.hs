{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.MaterialFull.PoisonousSubstance
  ( PoisonousSubstance(..)
  , poisonousSubstanceSchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PoisonousSubstance = PoisonousSubstance Bool
  deriving (Show, Eq)

poisonousSubstanceSchema :: FC.Fleece t => FC.Schema t PoisonousSubstance
poisonousSubstanceSchema =
  FC.coerceSchema FC.boolean