{-# LANGUAGE NoImplicitPrelude #-}

module StarTrek.Types.OrganizationBase.PrisonOrPenalColony
  ( PrisonOrPenalColony(..)
  , prisonOrPenalColonySchema
  ) where

import qualified Fleece.Core as FC
import Prelude (Bool, Eq, Show)

newtype PrisonOrPenalColony = PrisonOrPenalColony Bool
  deriving (Show, Eq)

prisonOrPenalColonySchema :: FC.Fleece t => FC.Schema t PrisonOrPenalColony
prisonOrPenalColonySchema =
  FC.coerceSchema FC.boolean