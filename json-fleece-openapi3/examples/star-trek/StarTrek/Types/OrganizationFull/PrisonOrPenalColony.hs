{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationFull.PrisonOrPenalColony
  ( PrisonOrPenalColony(..)
  , prisonOrPenalColonySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PrisonOrPenalColony = PrisonOrPenalColony Bool
  deriving (Show, Eq)

prisonOrPenalColonySchema :: FC.Fleece schema => schema PrisonOrPenalColony
prisonOrPenalColonySchema =
  FC.coerceSchema FC.boolean