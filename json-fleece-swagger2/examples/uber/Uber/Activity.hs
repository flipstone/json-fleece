{-# LANGUAGE NoImplicitPrelude #-}

module Uber.Activity
  ( Activity(..)
  , activitySchema
  ) where

import Fleece.Core ((#+))
import qualified Fleece.Core as FC
import Prelude (($), Eq, Maybe, Show)
import Uber.Activity.Uuid (Uuid, uuidSchema)

data Activity = Activity
  { uuid :: Maybe Uuid -- ^ Unique identifier for the activity
  }
  deriving (Eq, Show)

activitySchema :: FC.Fleece schema => schema Activity
activitySchema =
  FC.object $
    FC.constructor Activity
      #+ FC.optional "uuid" uuid uuidSchema