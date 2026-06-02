{-# LANGUAGE NoImplicitPrelude #-}

module SelectedItemsExample.Types.HealthStatus
  ( HealthStatus(..)
  , healthStatusSchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Show)
import qualified SelectedItemsExample.Types.HealthStatus.Status as Status

newtype HealthStatus = HealthStatus
  { status :: Status.Status
  }
  deriving (Eq, Show)

healthStatusSchema :: FC.Fleece t => FC.Schema t HealthStatus
healthStatusSchema =
  FC.object $
    FC.constructor HealthStatus
      #+ FC.required "status" status Status.statusSchema