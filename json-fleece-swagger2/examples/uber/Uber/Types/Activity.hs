{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Types.Activity
  ( Activity(..)
  , activitySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import qualified Uber.Types.Activity.Uuid as Uuid

data Activity = Activity
  { uuid :: Maybe Uuid.Uuid -- ^ Unique identifier for the activity
  }
  deriving (Eq, Show)

activitySchema :: FC.Fleece schema => schema Activity
activitySchema =
  FC.object $
    FC.constructor Activity
      #+ FC.optional "uuid" uuid Uuid.uuidSchema